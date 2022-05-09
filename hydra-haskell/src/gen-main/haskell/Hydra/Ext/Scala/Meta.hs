module Hydra.Ext.Scala.Meta where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

type PredefString = String

_PredefString = "hydra/ext/scala/meta.PredefString"

data ScalaSymbol 
  = ScalaSymbol {scalaSymbolName :: String}
  deriving (Eq, Ord, Read, Show)

_ScalaSymbol = "hydra/ext/scala/meta.ScalaSymbol"

_ScalaSymbol_name = "name"

data Tree 
  = TreeRef Ref
  | TreeStat Stat
  | TreeType Type
  | TreeBounds Type_Bounds
  | TreePat Pat
  | TreeMember Member
  | TreeCtor Ctor
  | TreeTemplate Template
  | TreeMod Mod
  | TreeEnumerator Enumerator
  | TreeImporter Importer
  | TreeImportee Importee
  | TreeCaseTree CaseTree
  | TreeSource Source
  | TreeQuasi Quasi
  deriving (Eq, Ord, Read, Show)

_Tree = "hydra/ext/scala/meta.Tree"

_Tree_ref = "ref"

_Tree_stat = "stat"

_Tree_type = "type"

_Tree_bounds = "bounds"

_Tree_pat = "pat"

_Tree_member = "member"

_Tree_ctor = "ctor"

_Tree_template = "template"

_Tree_mod = "mod"

_Tree_enumerator = "enumerator"

_Tree_importer = "importer"

_Tree_importee = "importee"

_Tree_caseTree = "caseTree"

_Tree_source = "source"

_Tree_quasi = "quasi"

data Ref 
  = RefName Name
  | RefInit Init
  deriving (Eq, Ord, Read, Show)

_Ref = "hydra/ext/scala/meta.Ref"

_Ref_name = "name"

_Ref_init = "init"

data Stat 
  = StatTerm Data
  | StatDecl Decl
  | StatDefn Defn
  | StatImportExport ImportExportStat
  deriving (Eq, Ord, Read, Show)

_Stat = "hydra/ext/scala/meta.Stat"

_Stat_term = "term"

_Stat_decl = "decl"

_Stat_defn = "defn"

_Stat_importExport = "importExport"

data Name 
  = NameValue String
  | NameAnonymous 
  | NameIndeterminate PredefString
  deriving (Eq, Ord, Read, Show)

_Name = "hydra/ext/scala/meta.Name"

_Name_value = "value"

_Name_anonymous = "anonymous"

_Name_indeterminate = "indeterminate"

data Lit 
  = LitNull 
  | LitInt Int
  | LitDouble Double
  | LitFloat Float
  | LitByte Int
  | LitShort Int
  | LitChar Int
  | LitLong Integer
  | LitBoolean Bool
  | LitUnit 
  | LitString String
  | LitSymbol ScalaSymbol
  deriving (Eq, Ord, Read, Show)

_Lit = "hydra/ext/scala/meta.Lit"

_Lit_null = "null"

_Lit_int = "int"

_Lit_double = "double"

_Lit_float = "float"

_Lit_byte = "byte"

_Lit_short = "short"

_Lit_char = "char"

_Lit_long = "long"

_Lit_boolean = "boolean"

_Lit_unit = "unit"

_Lit_string = "string"

_Lit_symbol = "symbol"

data Data 
  = DataLit Lit
  | DataRef Data_Ref
  | DataInterpolate Data_Interpolate
  | DataXml Data_Xml
  | DataApply Data_Apply
  | DataApplyUsing Data_ApplyUsing
  | DataApplyType Data_ApplyType
  | DataAssign Data_Assign
  | DataReturn Data_Return
  | DataThrow Data_Throw
  | DataAscribe Data_Ascribe
  | DataAnnotate Data_Annotate
  | DataTuple Data_Tuple
  | DataBlock Data_Block
  | DataEndMarker Data_EndMarker
  | DataIf Data_If
  | DataQuotedMacroExpr Data_QuotedMacroExpr
  | DataQuotedMacroType Data_QuotedMacroType
  | DataSplicedMacroExpr Data_SplicedMacroExpr
  | DataMatch Data_Match
  | DataTry Data_Try
  | DataTryWithHandler Data_TryWithHandler
  | DataFunctionData Data_FunctionData
  | DataPolyFunction Data_PolyFunction
  | DataPartialFunction Data_PartialFunction
  | DataWhile Data_While
  | DataDo Data_Do
  | DataFor Data_For
  | DataForYield Data_ForYield
  | DataNew Data_New
  | DataNewAnonymous Data_NewAnonymous
  | DataPlaceholder Data_Placeholder
  | DataEta Data_Eta
  | DataRepeated Data_Repeated
  | DataParam Data_Param
  deriving (Eq, Ord, Read, Show)

_Data = "hydra/ext/scala/meta.Data"

_Data_lit = "lit"

_Data_ref = "ref"

_Data_interpolate = "interpolate"

_Data_xml = "xml"

_Data_apply = "apply"

_Data_applyUsing = "applyUsing"

_Data_applyType = "applyType"

_Data_assign = "assign"

_Data_return = "return"

_Data_throw = "throw"

_Data_ascribe = "ascribe"

_Data_annotate = "annotate"

_Data_tuple = "tuple"

_Data_block = "block"

_Data_endMarker = "endMarker"

_Data_if = "if"

_Data_quotedMacroExpr = "quotedMacroExpr"

_Data_quotedMacroType = "quotedMacroType"

_Data_splicedMacroExpr = "splicedMacroExpr"

_Data_match = "match"

_Data_try = "try"

_Data_tryWithHandler = "tryWithHandler"

_Data_functionData = "functionData"

_Data_polyFunction = "polyFunction"

_Data_partialFunction = "partialFunction"

_Data_while = "while"

_Data_do = "do"

_Data_for = "for"

_Data_forYield = "forYield"

_Data_new = "new"

_Data_newAnonymous = "newAnonymous"

_Data_placeholder = "placeholder"

_Data_eta = "eta"

_Data_repeated = "repeated"

_Data_param = "param"

data Data_Ref 
  = Data_RefThis Data_This
  | Data_RefSuper Data_Super
  | Data_RefName Data_Name
  | Data_RefAnonymous Data_Anonymous
  | Data_RefSelect Data_Select
  | Data_RefApplyUnary Data_ApplyUnary
  deriving (Eq, Ord, Read, Show)

_Data_Ref = "hydra/ext/scala/meta.Data.Ref"

_Data_Ref_this = "this"

_Data_Ref_super = "super"

_Data_Ref_name = "name"

_Data_Ref_anonymous = "anonymous"

_Data_Ref_select = "select"

_Data_Ref_applyUnary = "applyUnary"

data Data_This 
  = Data_This {}
  deriving (Eq, Ord, Read, Show)

_Data_This = "hydra/ext/scala/meta.Data.This"

data Data_Super 
  = Data_Super {
    data_SuperThisp :: Name,
    data_SuperSuperp :: Name}
  deriving (Eq, Ord, Read, Show)

_Data_Super = "hydra/ext/scala/meta.Data.Super"

_Data_Super_thisp = "thisp"

_Data_Super_superp = "superp"

data Data_Name 
  = Data_Name {data_NameValue :: PredefString}
  deriving (Eq, Ord, Read, Show)

_Data_Name = "hydra/ext/scala/meta.Data.Name"

_Data_Name_value = "value"

data Data_Anonymous 
  = Data_Anonymous {}
  deriving (Eq, Ord, Read, Show)

_Data_Anonymous = "hydra/ext/scala/meta.Data.Anonymous"

data Data_Select 
  = Data_Select {
    data_SelectQual :: Data,
    data_SelectName :: Data_Name}
  deriving (Eq, Ord, Read, Show)

_Data_Select = "hydra/ext/scala/meta.Data.Select"

_Data_Select_qual = "qual"

_Data_Select_name = "name"

data Data_Interpolate 
  = Data_Interpolate {
    data_InterpolatePrefix :: Data_Name,
    data_InterpolateParts :: [Lit],
    data_InterpolateArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)

_Data_Interpolate = "hydra/ext/scala/meta.Data.Interpolate"

_Data_Interpolate_prefix = "prefix"

_Data_Interpolate_parts = "parts"

_Data_Interpolate_args = "args"

data Data_Xml 
  = Data_Xml {
    data_XmlParts :: [Lit],
    data_XmlArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)

_Data_Xml = "hydra/ext/scala/meta.Data.Xml"

_Data_Xml_parts = "parts"

_Data_Xml_args = "args"

data Data_Apply 
  = Data_Apply {
    data_ApplyFun :: Data,
    data_ApplyArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)

_Data_Apply = "hydra/ext/scala/meta.Data.Apply"

_Data_Apply_fun = "fun"

_Data_Apply_args = "args"

data Data_ApplyUsing 
  = Data_ApplyUsing {
    data_ApplyUsingFun :: Data,
    data_ApplyUsingTargs :: [Data]}
  deriving (Eq, Ord, Read, Show)

_Data_ApplyUsing = "hydra/ext/scala/meta.Data.ApplyUsing"

_Data_ApplyUsing_fun = "fun"

_Data_ApplyUsing_targs = "targs"

data Data_ApplyType 
  = Data_ApplyType {
    data_ApplyTypeLhs :: Data,
    data_ApplyTypeOp :: Data_Name,
    data_ApplyTypeTargs :: [Type],
    data_ApplyTypeArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)

_Data_ApplyType = "hydra/ext/scala/meta.Data.ApplyType"

_Data_ApplyType_lhs = "lhs"

_Data_ApplyType_op = "op"

_Data_ApplyType_targs = "targs"

_Data_ApplyType_args = "args"

data Data_ApplyInfix 
  = Data_ApplyInfix {
    data_ApplyInfixLhs :: Data,
    data_ApplyInfixOp :: Data_Name,
    data_ApplyInfixTargs :: [Type],
    data_ApplyInfixArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)

_Data_ApplyInfix = "hydra/ext/scala/meta.Data.ApplyInfix"

_Data_ApplyInfix_lhs = "lhs"

_Data_ApplyInfix_op = "op"

_Data_ApplyInfix_targs = "targs"

_Data_ApplyInfix_args = "args"

data Data_ApplyUnary 
  = Data_ApplyUnary {
    data_ApplyUnaryOp :: Data_Name,
    data_ApplyUnaryArg :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_ApplyUnary = "hydra/ext/scala/meta.Data.ApplyUnary"

_Data_ApplyUnary_op = "op"

_Data_ApplyUnary_arg = "arg"

data Data_Assign 
  = Data_Assign {
    data_AssignLhs :: Data,
    data_AssignRhs :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_Assign = "hydra/ext/scala/meta.Data.Assign"

_Data_Assign_lhs = "lhs"

_Data_Assign_rhs = "rhs"

data Data_Return 
  = Data_Return {data_ReturnExpr :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_Return = "hydra/ext/scala/meta.Data.Return"

_Data_Return_expr = "expr"

data Data_Throw 
  = Data_Throw {data_ThrowExpr :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_Throw = "hydra/ext/scala/meta.Data.Throw"

_Data_Throw_expr = "expr"

data Data_Ascribe 
  = Data_Ascribe {
    data_AscribeExpr :: Data,
    data_AscribeTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Data_Ascribe = "hydra/ext/scala/meta.Data.Ascribe"

_Data_Ascribe_expr = "expr"

_Data_Ascribe_tpe = "tpe"

data Data_Annotate 
  = Data_Annotate {
    data_AnnotateExpr :: Data,
    data_AnnotateAnnots :: [Mod_Annot]}
  deriving (Eq, Ord, Read, Show)

_Data_Annotate = "hydra/ext/scala/meta.Data.Annotate"

_Data_Annotate_expr = "expr"

_Data_Annotate_annots = "annots"

data Data_Tuple 
  = Data_Tuple {data_TupleArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)

_Data_Tuple = "hydra/ext/scala/meta.Data.Tuple"

_Data_Tuple_args = "args"

data Data_Block 
  = Data_Block {data_BlockStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)

_Data_Block = "hydra/ext/scala/meta.Data.Block"

_Data_Block_stats = "stats"

data Data_EndMarker 
  = Data_EndMarker {data_EndMarkerName :: Data_Name}
  deriving (Eq, Ord, Read, Show)

_Data_EndMarker = "hydra/ext/scala/meta.Data.EndMarker"

_Data_EndMarker_name = "name"

data Data_If 
  = Data_If {
    data_IfCond :: Data,
    data_IfThenp :: Data,
    data_IfElsep :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_If = "hydra/ext/scala/meta.Data.If"

_Data_If_cond = "cond"

_Data_If_thenp = "thenp"

_Data_If_elsep = "elsep"

data Data_QuotedMacroExpr 
  = Data_QuotedMacroExpr {data_QuotedMacroExprBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_QuotedMacroExpr = "hydra/ext/scala/meta.Data.QuotedMacroExpr"

_Data_QuotedMacroExpr_body = "body"

data Data_QuotedMacroType 
  = Data_QuotedMacroType {data_QuotedMacroTypeTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Data_QuotedMacroType = "hydra/ext/scala/meta.Data.QuotedMacroType"

_Data_QuotedMacroType_tpe = "tpe"

data Data_SplicedMacroExpr 
  = Data_SplicedMacroExpr {data_SplicedMacroExprBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_SplicedMacroExpr = "hydra/ext/scala/meta.Data.SplicedMacroExpr"

_Data_SplicedMacroExpr_body = "body"

data Data_Match 
  = Data_Match {
    data_MatchExpr :: Data,
    data_MatchCases :: [Case]}
  deriving (Eq, Ord, Read, Show)

_Data_Match = "hydra/ext/scala/meta.Data.Match"

_Data_Match_expr = "expr"

_Data_Match_cases = "cases"

data Data_Try 
  = Data_Try {
    data_TryExpr :: Data,
    data_TryCatchp :: [Case],
    data_TryFinallyp :: (Maybe Data)}
  deriving (Eq, Ord, Read, Show)

_Data_Try = "hydra/ext/scala/meta.Data.Try"

_Data_Try_expr = "expr"

_Data_Try_catchp = "catchp"

_Data_Try_finallyp = "finallyp"

data Data_TryWithHandler 
  = Data_TryWithHandler {
    data_TryWithHandlerExpr :: Data,
    data_TryWithHandlerCatchp :: Data,
    data_TryWithHandlerFinallyp :: (Maybe Data)}
  deriving (Eq, Ord, Read, Show)

_Data_TryWithHandler = "hydra/ext/scala/meta.Data.TryWithHandler"

_Data_TryWithHandler_expr = "expr"

_Data_TryWithHandler_catchp = "catchp"

_Data_TryWithHandler_finallyp = "finallyp"

data Data_FunctionData 
  = Data_FunctionDataContextFunction Data_ContextFunction
  | Data_FunctionDataFunction Data_Function
  deriving (Eq, Ord, Read, Show)

_Data_FunctionData = "hydra/ext/scala/meta.Data.FunctionData"

_Data_FunctionData_contextFunction = "contextFunction"

_Data_FunctionData_function = "function"

data Data_ContextFunction 
  = Data_ContextFunction {
    data_ContextFunctionParams :: [Data_Param],
    data_ContextFunctionBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_ContextFunction = "hydra/ext/scala/meta.Data.ContextFunction"

_Data_ContextFunction_params = "params"

_Data_ContextFunction_body = "body"

data Data_Function 
  = Data_Function {
    data_FunctionParams :: [Data_Param],
    data_FunctionBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_Function = "hydra/ext/scala/meta.Data.Function"

_Data_Function_params = "params"

_Data_Function_body = "body"

data Data_PolyFunction 
  = Data_PolyFunction {
    data_PolyFunctionTparams :: [Type_Param],
    data_PolyFunctionBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_PolyFunction = "hydra/ext/scala/meta.Data.PolyFunction"

_Data_PolyFunction_tparams = "tparams"

_Data_PolyFunction_body = "body"

data Data_PartialFunction 
  = Data_PartialFunction {data_PartialFunctionCases :: [Case]}
  deriving (Eq, Ord, Read, Show)

_Data_PartialFunction = "hydra/ext/scala/meta.Data.PartialFunction"

_Data_PartialFunction_cases = "cases"

data Data_While 
  = Data_While {
    data_WhileExpr :: Data,
    data_WhileBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_While = "hydra/ext/scala/meta.Data.While"

_Data_While_expr = "expr"

_Data_While_body = "body"

data Data_Do 
  = Data_Do {
    data_DoBody :: Data,
    data_DoExpr :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_Do = "hydra/ext/scala/meta.Data.Do"

_Data_Do_body = "body"

_Data_Do_expr = "expr"

data Data_For 
  = Data_For {data_ForEnums :: [Enumerator]}
  deriving (Eq, Ord, Read, Show)

_Data_For = "hydra/ext/scala/meta.Data.For"

_Data_For_enums = "enums"

data Data_ForYield 
  = Data_ForYield {data_ForYieldEnums :: [Enumerator]}
  deriving (Eq, Ord, Read, Show)

_Data_ForYield = "hydra/ext/scala/meta.Data.ForYield"

_Data_ForYield_enums = "enums"

data Data_New 
  = Data_New {data_NewInit :: Init}
  deriving (Eq, Ord, Read, Show)

_Data_New = "hydra/ext/scala/meta.Data.New"

_Data_New_init = "init"

data Data_NewAnonymous 
  = Data_NewAnonymous {data_NewAnonymousTempl :: Template}
  deriving (Eq, Ord, Read, Show)

_Data_NewAnonymous = "hydra/ext/scala/meta.Data.NewAnonymous"

_Data_NewAnonymous_templ = "templ"

data Data_Placeholder 
  = Data_Placeholder {}
  deriving (Eq, Ord, Read, Show)

_Data_Placeholder = "hydra/ext/scala/meta.Data.Placeholder"

data Data_Eta 
  = Data_Eta {data_EtaExpr :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_Eta = "hydra/ext/scala/meta.Data.Eta"

_Data_Eta_expr = "expr"

data Data_Repeated 
  = Data_Repeated {data_RepeatedExpr :: Data}
  deriving (Eq, Ord, Read, Show)

_Data_Repeated = "hydra/ext/scala/meta.Data.Repeated"

_Data_Repeated_expr = "expr"

data Data_Param 
  = Data_Param {
    data_ParamMods :: [Mod],
    data_ParamName :: Name,
    data_ParamDecltpe :: (Maybe Type),
    data_ParamDefault :: (Maybe Data)}
  deriving (Eq, Ord, Read, Show)

_Data_Param = "hydra/ext/scala/meta.Data.Param"

_Data_Param_mods = "mods"

_Data_Param_name = "name"

_Data_Param_decltpe = "decltpe"

_Data_Param_default = "default"

data Type 
  = TypeRef Type_Ref
  | TypeAnonymousName Type_AnonymousName
  | TypeApply Type_Apply
  | TypeApplyInfix Type_ApplyInfix
  | TypeFunctionType Type_FunctionType
  | TypePolyFunction Type_PolyFunction
  | TypeImplicitFunction Type_ImplicitFunction
  | TypeTuple Type_Tuple
  | TypeWith Type_With
  | TypeAnd Type_And
  | TypeOr Type_Or
  | TypeRefine Type_Refine
  | TypeExistential Type_Existential
  | TypeAnnotate Type_Annotate
  | TypeLambda Type_Lambda
  | TypeMacro Type_Macro
  | TypeMethod Type_Method
  | TypePlaceholder Type_Placeholder
  | TypeByName Type_ByName
  | TypeRepeated Type_Repeated
  | TypeVar Type_Var
  | TypeTypedParam Type_TypedParam
  | TypeMatch Type_Match
  deriving (Eq, Ord, Read, Show)

_Type = "hydra/ext/scala/meta.Type"

_Type_ref = "ref"

_Type_anonymousName = "anonymousName"

_Type_apply = "apply"

_Type_applyInfix = "applyInfix"

_Type_functionType = "functionType"

_Type_polyFunction = "polyFunction"

_Type_implicitFunction = "implicitFunction"

_Type_tuple = "tuple"

_Type_with = "with"

_Type_and = "and"

_Type_or = "or"

_Type_refine = "refine"

_Type_existential = "existential"

_Type_annotate = "annotate"

_Type_lambda = "lambda"

_Type_macro = "macro"

_Type_method = "method"

_Type_placeholder = "placeholder"

_Type_byName = "byName"

_Type_repeated = "repeated"

_Type_var = "var"

_Type_typedParam = "typedParam"

_Type_match = "match"

data Type_Ref 
  = Type_RefName Type_Name
  | Type_RefSelect Type_Select
  | Type_RefProject Type_Project
  | Type_RefSingleton Type_Singleton
  deriving (Eq, Ord, Read, Show)

_Type_Ref = "hydra/ext/scala/meta.Type.Ref"

_Type_Ref_name = "name"

_Type_Ref_select = "select"

_Type_Ref_project = "project"

_Type_Ref_singleton = "singleton"

data Type_Name 
  = Type_Name {type_NameValue :: String}
  deriving (Eq, Ord, Read, Show)

_Type_Name = "hydra/ext/scala/meta.Type.Name"

_Type_Name_value = "value"

data Type_AnonymousName 
  = Type_AnonymousName {}
  deriving (Eq, Ord, Read, Show)

_Type_AnonymousName = "hydra/ext/scala/meta.Type.AnonymousName"

data Type_Select 
  = Type_Select {
    type_SelectQual :: Data_Ref,
    type_SelectName :: Type_Name}
  deriving (Eq, Ord, Read, Show)

_Type_Select = "hydra/ext/scala/meta.Type.Select"

_Type_Select_qual = "qual"

_Type_Select_name = "name"

data Type_Project 
  = Type_Project {
    type_ProjectQual :: Type,
    type_ProjectName :: Type_Name}
  deriving (Eq, Ord, Read, Show)

_Type_Project = "hydra/ext/scala/meta.Type.Project"

_Type_Project_qual = "qual"

_Type_Project_name = "name"

data Type_Singleton 
  = Type_Singleton {type_SingletonRef :: Data_Ref}
  deriving (Eq, Ord, Read, Show)

_Type_Singleton = "hydra/ext/scala/meta.Type.Singleton"

_Type_Singleton_ref = "ref"

data Type_Apply 
  = Type_Apply {
    type_ApplyTpe :: Type,
    type_ApplyArgs :: [Type]}
  deriving (Eq, Ord, Read, Show)

_Type_Apply = "hydra/ext/scala/meta.Type.Apply"

_Type_Apply_tpe = "tpe"

_Type_Apply_args = "args"

data Type_ApplyInfix 
  = Type_ApplyInfix {
    type_ApplyInfixLhs :: Type,
    type_ApplyInfixOp :: Type_Name,
    type_ApplyInfixRhs :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_ApplyInfix = "hydra/ext/scala/meta.Type.ApplyInfix"

_Type_ApplyInfix_lhs = "lhs"

_Type_ApplyInfix_op = "op"

_Type_ApplyInfix_rhs = "rhs"

data Type_FunctionType 
  = Type_FunctionTypeFunction Type_Function
  | Type_FunctionTypeContextFunction Type_ContextFunction
  deriving (Eq, Ord, Read, Show)

_Type_FunctionType = "hydra/ext/scala/meta.Type.FunctionType"

_Type_FunctionType_function = "function"

_Type_FunctionType_contextFunction = "contextFunction"

data Type_Function 
  = Type_Function {
    type_FunctionParams :: [Type],
    type_FunctionRes :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Function = "hydra/ext/scala/meta.Type.Function"

_Type_Function_params = "params"

_Type_Function_res = "res"

data Type_PolyFunction 
  = Type_PolyFunction {
    type_PolyFunctionTparams :: [Type_Param],
    type_PolyFunctionTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_PolyFunction = "hydra/ext/scala/meta.Type.PolyFunction"

_Type_PolyFunction_tparams = "tparams"

_Type_PolyFunction_tpe = "tpe"

data Type_ContextFunction 
  = Type_ContextFunction {
    type_ContextFunctionParams :: [Type],
    type_ContextFunctionRes :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_ContextFunction = "hydra/ext/scala/meta.Type.ContextFunction"

_Type_ContextFunction_params = "params"

_Type_ContextFunction_res = "res"

data Type_ImplicitFunction 
  = Type_ImplicitFunction {
    type_ImplicitFunctionParams :: [Type],
    type_ImplicitFunctionRes :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_ImplicitFunction = "hydra/ext/scala/meta.Type.ImplicitFunction"

_Type_ImplicitFunction_params = "params"

_Type_ImplicitFunction_res = "res"

data Type_Tuple 
  = Type_Tuple {type_TupleArgs :: [Type]}
  deriving (Eq, Ord, Read, Show)

_Type_Tuple = "hydra/ext/scala/meta.Type.Tuple"

_Type_Tuple_args = "args"

data Type_With 
  = Type_With {
    type_WithLhs :: Type,
    type_WithRhs :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_With = "hydra/ext/scala/meta.Type.With"

_Type_With_lhs = "lhs"

_Type_With_rhs = "rhs"

data Type_And 
  = Type_And {
    type_AndLhs :: Type,
    type_AndRhs :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_And = "hydra/ext/scala/meta.Type.And"

_Type_And_lhs = "lhs"

_Type_And_rhs = "rhs"

data Type_Or 
  = Type_Or {
    type_OrLhs :: Type,
    type_OrRhs :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Or = "hydra/ext/scala/meta.Type.Or"

_Type_Or_lhs = "lhs"

_Type_Or_rhs = "rhs"

data Type_Refine 
  = Type_Refine {
    type_RefineTpe :: (Maybe Type),
    type_RefineStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)

_Type_Refine = "hydra/ext/scala/meta.Type.Refine"

_Type_Refine_tpe = "tpe"

_Type_Refine_stats = "stats"

data Type_Existential 
  = Type_Existential {
    type_ExistentialTpe :: Type,
    type_ExistentialStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)

_Type_Existential = "hydra/ext/scala/meta.Type.Existential"

_Type_Existential_tpe = "tpe"

_Type_Existential_stats = "stats"

data Type_Annotate 
  = Type_Annotate {
    type_AnnotateTpe :: Type,
    type_AnnotateAnnots :: [Mod_Annot]}
  deriving (Eq, Ord, Read, Show)

_Type_Annotate = "hydra/ext/scala/meta.Type.Annotate"

_Type_Annotate_tpe = "tpe"

_Type_Annotate_annots = "annots"

data Type_Lambda 
  = Type_Lambda {
    type_LambdaTparams :: [Type_Param],
    type_LambdaTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Lambda = "hydra/ext/scala/meta.Type.Lambda"

_Type_Lambda_tparams = "tparams"

_Type_Lambda_tpe = "tpe"

data Type_Macro 
  = Type_Macro {type_MacroBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Type_Macro = "hydra/ext/scala/meta.Type.Macro"

_Type_Macro_body = "body"

data Type_Method 
  = Type_Method {
    type_MethodParamss :: [[Data_Param]],
    type_MethodTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Method = "hydra/ext/scala/meta.Type.Method"

_Type_Method_paramss = "paramss"

_Type_Method_tpe = "tpe"

data Type_Placeholder 
  = Type_Placeholder {type_PlaceholderBounds :: Type_Bounds}
  deriving (Eq, Ord, Read, Show)

_Type_Placeholder = "hydra/ext/scala/meta.Type.Placeholder"

_Type_Placeholder_bounds = "bounds"

data Type_Bounds 
  = Type_Bounds {
    type_BoundsLo :: (Maybe Type),
    type_BoundsHi :: (Maybe Type)}
  deriving (Eq, Ord, Read, Show)

_Type_Bounds = "hydra/ext/scala/meta.Type.Bounds"

_Type_Bounds_lo = "lo"

_Type_Bounds_hi = "hi"

data Type_ByName 
  = Type_ByName {type_ByNameTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_ByName = "hydra/ext/scala/meta.Type.ByName"

_Type_ByName_tpe = "tpe"

data Type_Repeated 
  = Type_Repeated {type_RepeatedTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Repeated = "hydra/ext/scala/meta.Type.Repeated"

_Type_Repeated_tpe = "tpe"

data Type_Var 
  = Type_Var {type_VarName :: Type_Name}
  deriving (Eq, Ord, Read, Show)

_Type_Var = "hydra/ext/scala/meta.Type.Var"

_Type_Var_name = "name"

data Type_TypedParam 
  = Type_TypedParam {
    type_TypedParamName :: Name,
    type_TypedParamTyp :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_TypedParam = "hydra/ext/scala/meta.Type.TypedParam"

_Type_TypedParam_name = "name"

_Type_TypedParam_typ = "typ"

data Type_Param 
  = Type_Param {
    type_ParamMods :: [Mod],
    type_ParamName :: Name,
    type_ParamTparams :: [Type_Param],
    type_ParamTbounds :: [Type_Bounds],
    type_ParamVbounds :: [Type],
    type_ParamCbounds :: [Type]}
  deriving (Eq, Ord, Read, Show)

_Type_Param = "hydra/ext/scala/meta.Type.Param"

_Type_Param_mods = "mods"

_Type_Param_name = "name"

_Type_Param_tparams = "tparams"

_Type_Param_tbounds = "tbounds"

_Type_Param_vbounds = "vbounds"

_Type_Param_cbounds = "cbounds"

data Type_Match 
  = Type_Match {
    type_MatchTpe :: Type,
    type_MatchCases :: [TypeCase]}
  deriving (Eq, Ord, Read, Show)

_Type_Match = "hydra/ext/scala/meta.Type.Match"

_Type_Match_tpe = "tpe"

_Type_Match_cases = "cases"

data Pat 
  = PatVar Pat_Var
  | PatWildcard 
  | PatSeqWildcard 
  | PatBind Pat_Bind
  | PatAlternative Pat_Alternative
  | PatTuple Pat_Tuple
  | PatRepeated Pat_Repeated
  | PatExtract Pat_Extract
  | PatExtractInfix Pat_ExtractInfix
  | PatInterpolate Pat_Interpolate
  | PatXml Pat_Xml
  | PatTyped Pat_Typed
  | PatMacro Pat_Macro
  | PatGiven Pat_Given
  deriving (Eq, Ord, Read, Show)

_Pat = "hydra/ext/scala/meta.Pat"

_Pat_var = "var"

_Pat_wildcard = "wildcard"

_Pat_seqWildcard = "seqWildcard"

_Pat_bind = "bind"

_Pat_alternative = "alternative"

_Pat_tuple = "tuple"

_Pat_repeated = "repeated"

_Pat_extract = "extract"

_Pat_extractInfix = "extractInfix"

_Pat_interpolate = "interpolate"

_Pat_xml = "xml"

_Pat_typed = "typed"

_Pat_macro = "macro"

_Pat_given = "given"

data Pat_Var 
  = Pat_Var {pat_VarName :: Data_Name}
  deriving (Eq, Ord, Read, Show)

_Pat_Var = "hydra/ext/scala/meta.Pat.Var"

_Pat_Var_name = "name"

data Pat_Bind 
  = Pat_Bind {
    pat_BindLhs :: Pat,
    pat_BindRhs :: Pat}
  deriving (Eq, Ord, Read, Show)

_Pat_Bind = "hydra/ext/scala/meta.Pat.Bind"

_Pat_Bind_lhs = "lhs"

_Pat_Bind_rhs = "rhs"

data Pat_Alternative 
  = Pat_Alternative {
    pat_AlternativeLhs :: Pat,
    pat_AlternativeRhs :: Pat}
  deriving (Eq, Ord, Read, Show)

_Pat_Alternative = "hydra/ext/scala/meta.Pat.Alternative"

_Pat_Alternative_lhs = "lhs"

_Pat_Alternative_rhs = "rhs"

data Pat_Tuple 
  = Pat_Tuple {pat_TupleArgs :: [Pat]}
  deriving (Eq, Ord, Read, Show)

_Pat_Tuple = "hydra/ext/scala/meta.Pat.Tuple"

_Pat_Tuple_args = "args"

data Pat_Repeated 
  = Pat_Repeated {pat_RepeatedName :: Data_Name}
  deriving (Eq, Ord, Read, Show)

_Pat_Repeated = "hydra/ext/scala/meta.Pat.Repeated"

_Pat_Repeated_name = "name"

data Pat_Extract 
  = Pat_Extract {
    pat_ExtractFun :: Data,
    pat_ExtractArgs :: [Pat]}
  deriving (Eq, Ord, Read, Show)

_Pat_Extract = "hydra/ext/scala/meta.Pat.Extract"

_Pat_Extract_fun = "fun"

_Pat_Extract_args = "args"

data Pat_ExtractInfix 
  = Pat_ExtractInfix {
    pat_ExtractInfixLhs :: Pat,
    pat_ExtractInfixOp :: Data_Name,
    pat_ExtractInfixRhs :: [Pat]}
  deriving (Eq, Ord, Read, Show)

_Pat_ExtractInfix = "hydra/ext/scala/meta.Pat.ExtractInfix"

_Pat_ExtractInfix_lhs = "lhs"

_Pat_ExtractInfix_op = "op"

_Pat_ExtractInfix_rhs = "rhs"

data Pat_Interpolate 
  = Pat_Interpolate {
    pat_InterpolatePrefix :: Data_Name,
    pat_InterpolateParts :: [Lit]}
  deriving (Eq, Ord, Read, Show)

_Pat_Interpolate = "hydra/ext/scala/meta.Pat.Interpolate"

_Pat_Interpolate_prefix = "prefix"

_Pat_Interpolate_parts = "parts"

data Pat_Xml 
  = Pat_Xml {
    pat_XmlParts :: [Lit],
    pat_XmlArgs :: [Pat]}
  deriving (Eq, Ord, Read, Show)

_Pat_Xml = "hydra/ext/scala/meta.Pat.Xml"

_Pat_Xml_parts = "parts"

_Pat_Xml_args = "args"

data Pat_Typed 
  = Pat_Typed {
    pat_TypedLhs :: Pat,
    pat_TypedRhs :: Type}
  deriving (Eq, Ord, Read, Show)

_Pat_Typed = "hydra/ext/scala/meta.Pat.Typed"

_Pat_Typed_lhs = "lhs"

_Pat_Typed_rhs = "rhs"

data Pat_Macro 
  = Pat_Macro {pat_MacroBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Pat_Macro = "hydra/ext/scala/meta.Pat.Macro"

_Pat_Macro_body = "body"

data Pat_Given 
  = Pat_Given {pat_GivenTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Pat_Given = "hydra/ext/scala/meta.Pat.Given"

_Pat_Given_tpe = "tpe"

data Member 
  = MemberTerm Member_Data
  | MemberType Member_Type
  | MemberTermParam Data_Param
  | MemberTypeParam Type_Param
  | MemberSelf Self
  deriving (Eq, Ord, Read, Show)

_Member = "hydra/ext/scala/meta.Member"

_Member_term = "term"

_Member_type = "type"

_Member_termParam = "termParam"

_Member_typeParam = "typeParam"

_Member_self = "self"

data Member_Data 
  = Member_DataPkg Pkg
  | Member_DataObject Pkg_Object
  deriving (Eq, Ord, Read, Show)

_Member_Data = "hydra/ext/scala/meta.Member.Data"

_Member_Data_pkg = "pkg"

_Member_Data_object = "object"

data Member_Type 
  = Member_Type {member_TypeName :: Type_Name}
  deriving (Eq, Ord, Read, Show)

_Member_Type = "hydra/ext/scala/meta.Member.Type"

_Member_Type_name = "name"

data Decl 
  = DeclVal Decl_Val
  | DeclVar Decl_Var
  | DeclDef Decl_Def
  | DeclType Decl_Type
  | DeclGiven Decl_Given
  deriving (Eq, Ord, Read, Show)

_Decl = "hydra/ext/scala/meta.Decl"

_Decl_val = "val"

_Decl_var = "var"

_Decl_def = "def"

_Decl_type = "type"

_Decl_given = "given"

data Decl_Val 
  = Decl_Val {
    decl_ValMods :: [Mod],
    decl_ValPats :: [Pat],
    decl_ValDecltpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Decl_Val = "hydra/ext/scala/meta.Decl.Val"

_Decl_Val_mods = "mods"

_Decl_Val_pats = "pats"

_Decl_Val_decltpe = "decltpe"

data Decl_Var 
  = Decl_Var {
    decl_VarMods :: [Mod],
    decl_VarPats :: [Pat],
    decl_VarDecltpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Decl_Var = "hydra/ext/scala/meta.Decl.Var"

_Decl_Var_mods = "mods"

_Decl_Var_pats = "pats"

_Decl_Var_decltpe = "decltpe"

data Decl_Def 
  = Decl_Def {
    decl_DefMods :: [Mod],
    decl_DefName :: Data_Name,
    decl_DefTparams :: [Type_Param],
    decl_DefParamss :: [[Data_Param]],
    decl_DefDecltpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Decl_Def = "hydra/ext/scala/meta.Decl.Def"

_Decl_Def_mods = "mods"

_Decl_Def_name = "name"

_Decl_Def_tparams = "tparams"

_Decl_Def_paramss = "paramss"

_Decl_Def_decltpe = "decltpe"

data Decl_Type 
  = Decl_Type {
    decl_TypeMods :: [Mod],
    decl_TypeName :: Type_Name,
    decl_TypeTparams :: [Type_Param],
    decl_TypeBounds :: Type_Bounds}
  deriving (Eq, Ord, Read, Show)

_Decl_Type = "hydra/ext/scala/meta.Decl.Type"

_Decl_Type_mods = "mods"

_Decl_Type_name = "name"

_Decl_Type_tparams = "tparams"

_Decl_Type_bounds = "bounds"

data Decl_Given 
  = Decl_Given {
    decl_GivenMods :: [Mod],
    decl_GivenName :: Data_Name,
    decl_GivenTparams :: [Type_Param],
    decl_GivenSparams :: [[Data_Param]],
    decl_GivenDecltpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Decl_Given = "hydra/ext/scala/meta.Decl.Given"

_Decl_Given_mods = "mods"

_Decl_Given_name = "name"

_Decl_Given_tparams = "tparams"

_Decl_Given_sparams = "sparams"

_Decl_Given_decltpe = "decltpe"

data Defn 
  = DefnVal Defn_Val
  | DefnVar Defn_Var
  | DefnGiven Defn_Given
  | DefnEnum Defn_Enum
  | DefnEnumCase Defn_EnumCase
  | DefnRepeatedEnumCase Defn_RepeatedEnumCase
  | DefnGivenAlias Defn_GivenAlias
  | DefnExtensionGroup Defn_ExtensionGroup
  | DefnDef Defn_Def
  | DefnMacro Defn_Macro
  | DefnType Defn_Type
  | DefnClass Defn_Class
  | DefnTrait Defn_Trait
  | DefnObject Defn_Object
  deriving (Eq, Ord, Read, Show)

_Defn = "hydra/ext/scala/meta.Defn"

_Defn_val = "val"

_Defn_var = "var"

_Defn_given = "given"

_Defn_enum = "enum"

_Defn_enumCase = "enumCase"

_Defn_repeatedEnumCase = "repeatedEnumCase"

_Defn_givenAlias = "givenAlias"

_Defn_extensionGroup = "extensionGroup"

_Defn_def = "def"

_Defn_macro = "macro"

_Defn_type = "type"

_Defn_class = "class"

_Defn_trait = "trait"

_Defn_object = "object"

data Defn_Val 
  = Defn_Val {
    defn_ValMods :: [Mod],
    defn_ValPats :: [Pat],
    defn_ValDecltpe :: (Maybe Type),
    defn_ValRhs :: Data}
  deriving (Eq, Ord, Read, Show)

_Defn_Val = "hydra/ext/scala/meta.Defn.Val"

_Defn_Val_mods = "mods"

_Defn_Val_pats = "pats"

_Defn_Val_decltpe = "decltpe"

_Defn_Val_rhs = "rhs"

data Defn_Var 
  = Defn_Var {
    defn_VarMods :: [Mod],
    defn_VarPats :: [Pat],
    defn_VarDecltpe :: Type,
    defn_VarRhs :: (Maybe Data)}
  deriving (Eq, Ord, Read, Show)

_Defn_Var = "hydra/ext/scala/meta.Defn.Var"

_Defn_Var_mods = "mods"

_Defn_Var_pats = "pats"

_Defn_Var_decltpe = "decltpe"

_Defn_Var_rhs = "rhs"

data Defn_Given 
  = Defn_Given {
    defn_GivenMods :: [Mod],
    defn_GivenName :: Name,
    defn_GivenTparams :: [[Type_Param]],
    defn_GivenSparams :: [[Data_Param]],
    defn_GivenTempl :: Template}
  deriving (Eq, Ord, Read, Show)

_Defn_Given = "hydra/ext/scala/meta.Defn.Given"

_Defn_Given_mods = "mods"

_Defn_Given_name = "name"

_Defn_Given_tparams = "tparams"

_Defn_Given_sparams = "sparams"

_Defn_Given_templ = "templ"

data Defn_Enum 
  = Defn_Enum {
    defn_EnumMods :: [Mod],
    defn_EnumName :: Type_Name,
    defn_EnumTparams :: [Type_Param],
    defn_EnumCtor :: Ctor_Primary,
    defn_EnumTemplate :: Template}
  deriving (Eq, Ord, Read, Show)

_Defn_Enum = "hydra/ext/scala/meta.Defn.Enum"

_Defn_Enum_mods = "mods"

_Defn_Enum_name = "name"

_Defn_Enum_tparams = "tparams"

_Defn_Enum_ctor = "ctor"

_Defn_Enum_template = "template"

data Defn_EnumCase 
  = Defn_EnumCase {
    defn_EnumCaseMods :: [Mod],
    defn_EnumCaseName :: Data_Name,
    defn_EnumCaseTparams :: [Type_Param],
    defn_EnumCaseCtor :: Ctor_Primary,
    defn_EnumCaseInits :: [Init]}
  deriving (Eq, Ord, Read, Show)

_Defn_EnumCase = "hydra/ext/scala/meta.Defn.EnumCase"

_Defn_EnumCase_mods = "mods"

_Defn_EnumCase_name = "name"

_Defn_EnumCase_tparams = "tparams"

_Defn_EnumCase_ctor = "ctor"

_Defn_EnumCase_inits = "inits"

data Defn_RepeatedEnumCase 
  = Defn_RepeatedEnumCase {
    defn_RepeatedEnumCaseMods :: [Mod],
    defn_RepeatedEnumCaseCases :: [Data_Name]}
  deriving (Eq, Ord, Read, Show)

_Defn_RepeatedEnumCase = "hydra/ext/scala/meta.Defn.RepeatedEnumCase"

_Defn_RepeatedEnumCase_mods = "mods"

_Defn_RepeatedEnumCase_cases = "cases"

data Defn_GivenAlias 
  = Defn_GivenAlias {
    defn_GivenAliasMods :: [Mod],
    defn_GivenAliasName :: Name,
    defn_GivenAliasTparams :: [[Type_Param]],
    defn_GivenAliasSparams :: [[Data_Param]],
    defn_GivenAliasDecltpe :: Type,
    defn_GivenAliasBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Defn_GivenAlias = "hydra/ext/scala/meta.Defn.GivenAlias"

_Defn_GivenAlias_mods = "mods"

_Defn_GivenAlias_name = "name"

_Defn_GivenAlias_tparams = "tparams"

_Defn_GivenAlias_sparams = "sparams"

_Defn_GivenAlias_decltpe = "decltpe"

_Defn_GivenAlias_body = "body"

data Defn_ExtensionGroup 
  = Defn_ExtensionGroup {
    defn_ExtensionGroupTparams :: [Type_Param],
    defn_ExtensionGroupParmss :: [[Data_Param]],
    defn_ExtensionGroupBody :: Stat}
  deriving (Eq, Ord, Read, Show)

_Defn_ExtensionGroup = "hydra/ext/scala/meta.Defn.ExtensionGroup"

_Defn_ExtensionGroup_tparams = "tparams"

_Defn_ExtensionGroup_parmss = "parmss"

_Defn_ExtensionGroup_body = "body"

data Defn_Def 
  = Defn_Def {
    defn_DefMods :: [Mod],
    defn_DefName :: Data_Name,
    defn_DefTparams :: [Type_Param],
    defn_DefParamss :: [[Data_Param]],
    defn_DefDecltpe :: (Maybe Type),
    defn_DefBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Defn_Def = "hydra/ext/scala/meta.Defn.Def"

_Defn_Def_mods = "mods"

_Defn_Def_name = "name"

_Defn_Def_tparams = "tparams"

_Defn_Def_paramss = "paramss"

_Defn_Def_decltpe = "decltpe"

_Defn_Def_body = "body"

data Defn_Macro 
  = Defn_Macro {
    defn_MacroMods :: [Mod],
    defn_MacroName :: Data_Name,
    defn_MacroTparams :: [Type_Param],
    defn_MacroParamss :: [[Data_Param]],
    defn_MacroDecltpe :: (Maybe Type),
    defn_MacroBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Defn_Macro = "hydra/ext/scala/meta.Defn.Macro"

_Defn_Macro_mods = "mods"

_Defn_Macro_name = "name"

_Defn_Macro_tparams = "tparams"

_Defn_Macro_paramss = "paramss"

_Defn_Macro_decltpe = "decltpe"

_Defn_Macro_body = "body"

data Defn_Type 
  = Defn_Type {
    defn_TypeMods :: [Mod],
    defn_TypeName :: Type_Name,
    defn_TypeTparams :: [Type_Param],
    defn_TypeBody :: Type}
  deriving (Eq, Ord, Read, Show)

_Defn_Type = "hydra/ext/scala/meta.Defn.Type"

_Defn_Type_mods = "mods"

_Defn_Type_name = "name"

_Defn_Type_tparams = "tparams"

_Defn_Type_body = "body"

data Defn_Class 
  = Defn_Class {
    defn_ClassMods :: [Mod],
    defn_ClassName :: Type_Name,
    defn_ClassTparams :: [Type_Param],
    defn_ClassCtor :: Ctor_Primary,
    defn_ClassTemplate :: Template}
  deriving (Eq, Ord, Read, Show)

_Defn_Class = "hydra/ext/scala/meta.Defn.Class"

_Defn_Class_mods = "mods"

_Defn_Class_name = "name"

_Defn_Class_tparams = "tparams"

_Defn_Class_ctor = "ctor"

_Defn_Class_template = "template"

data Defn_Trait 
  = Defn_Trait {
    defn_TraitMods :: [Mod],
    defn_TraitName :: Type_Name,
    defn_TraitTparams :: [Type_Param],
    defn_TraitCtor :: Ctor_Primary,
    defn_TraitTemplate :: Template}
  deriving (Eq, Ord, Read, Show)

_Defn_Trait = "hydra/ext/scala/meta.Defn.Trait"

_Defn_Trait_mods = "mods"

_Defn_Trait_name = "name"

_Defn_Trait_tparams = "tparams"

_Defn_Trait_ctor = "ctor"

_Defn_Trait_template = "template"

data Defn_Object 
  = Defn_Object {defn_ObjectName :: Data_Name}
  deriving (Eq, Ord, Read, Show)

_Defn_Object = "hydra/ext/scala/meta.Defn.Object"

_Defn_Object_name = "name"

data Pkg 
  = Pkg {
    pkgName :: Data_Name,
    pkgRef :: Data_Ref,
    pkgStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)

_Pkg = "hydra/ext/scala/meta.Pkg"

_Pkg_name = "name"

_Pkg_ref = "ref"

_Pkg_stats = "stats"

data Pkg_Object 
  = Pkg_Object {
    pkg_ObjectMods :: [Mod],
    pkg_ObjectName :: Data_Name,
    pkg_ObjectTemplate :: Template}
  deriving (Eq, Ord, Read, Show)

_Pkg_Object = "hydra/ext/scala/meta.Pkg.Object"

_Pkg_Object_mods = "mods"

_Pkg_Object_name = "name"

_Pkg_Object_template = "template"

data Ctor 
  = CtorPrimary Ctor_Primary
  | CtorSecondary Ctor_Secondary
  deriving (Eq, Ord, Read, Show)

_Ctor = "hydra/ext/scala/meta.Ctor"

_Ctor_primary = "primary"

_Ctor_secondary = "secondary"

data Ctor_Primary 
  = Ctor_Primary {
    ctor_PrimaryMods :: [Mod],
    ctor_PrimaryName :: Name,
    ctor_PrimaryParamss :: [[Data_Param]]}
  deriving (Eq, Ord, Read, Show)

_Ctor_Primary = "hydra/ext/scala/meta.Ctor.Primary"

_Ctor_Primary_mods = "mods"

_Ctor_Primary_name = "name"

_Ctor_Primary_paramss = "paramss"

data Ctor_Secondary 
  = Ctor_Secondary {
    ctor_SecondaryMods :: [Mod],
    ctor_SecondaryName :: Name,
    ctor_SecondaryParamss :: [[Data_Param]],
    ctor_SecondaryInit :: Init,
    ctor_SecondaryStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)

_Ctor_Secondary = "hydra/ext/scala/meta.Ctor.Secondary"

_Ctor_Secondary_mods = "mods"

_Ctor_Secondary_name = "name"

_Ctor_Secondary_paramss = "paramss"

_Ctor_Secondary_init = "init"

_Ctor_Secondary_stats = "stats"

data Init 
  = Init {
    initTpe :: Type,
    initName :: Name,
    initArgss :: [[Data]]}
  deriving (Eq, Ord, Read, Show)

_Init = "hydra/ext/scala/meta.Init"

_Init_tpe = "tpe"

_Init_name = "name"

_Init_argss = "argss"

data Self 
  = Self {}
  deriving (Eq, Ord, Read, Show)

_Self = "hydra/ext/scala/meta.Self"

data Template 
  = Template {
    templateEarly :: [Stat],
    templateInits :: [Init],
    templateSelf :: Self,
    templateStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)

_Template = "hydra/ext/scala/meta.Template"

_Template_early = "early"

_Template_inits = "inits"

_Template_self = "self"

_Template_stats = "stats"

data Mod 
  = ModAnnot Mod_Annot
  | ModPrivate Mod_Private
  | ModProtected Mod_Protected
  | ModImplicit 
  | ModFinal 
  | ModSealed 
  | ModOpen 
  | ModSuper 
  | ModOverride 
  | ModCase 
  | ModAbstract 
  | ModCovariant 
  | ModContravariant 
  | ModLazy 
  | ModValParam 
  | ModVarParam 
  | ModInfix 
  | ModInline 
  | ModUsing 
  | ModOpaque 
  | ModTransparent 
  deriving (Eq, Ord, Read, Show)

_Mod = "hydra/ext/scala/meta.Mod"

_Mod_annot = "annot"

_Mod_private = "private"

_Mod_protected = "protected"

_Mod_implicit = "implicit"

_Mod_final = "final"

_Mod_sealed = "sealed"

_Mod_open = "open"

_Mod_super = "super"

_Mod_override = "override"

_Mod_case = "case"

_Mod_abstract = "abstract"

_Mod_covariant = "covariant"

_Mod_contravariant = "contravariant"

_Mod_lazy = "lazy"

_Mod_valParam = "valParam"

_Mod_varParam = "varParam"

_Mod_infix = "infix"

_Mod_inline = "inline"

_Mod_using = "using"

_Mod_opaque = "opaque"

_Mod_transparent = "transparent"

data Mod_Annot 
  = Mod_Annot {mod_AnnotInit :: Init}
  deriving (Eq, Ord, Read, Show)

_Mod_Annot = "hydra/ext/scala/meta.Mod.Annot"

_Mod_Annot_init = "init"

data Mod_Private 
  = Mod_Private {mod_PrivateWithin :: Ref}
  deriving (Eq, Ord, Read, Show)

_Mod_Private = "hydra/ext/scala/meta.Mod.Private"

_Mod_Private_within = "within"

data Mod_Protected 
  = Mod_Protected {mod_ProtectedWithin :: Ref}
  deriving (Eq, Ord, Read, Show)

_Mod_Protected = "hydra/ext/scala/meta.Mod.Protected"

_Mod_Protected_within = "within"

data Enumerator 
  = EnumeratorGenerator Enumerator_Generator
  | EnumeratorCaseGenerator Enumerator_CaseGenerator
  | EnumeratorVal Enumerator_Val
  | EnumeratorGuard Enumerator_Guard
  deriving (Eq, Ord, Read, Show)

_Enumerator = "hydra/ext/scala/meta.Enumerator"

_Enumerator_generator = "generator"

_Enumerator_caseGenerator = "caseGenerator"

_Enumerator_val = "val"

_Enumerator_guard = "guard"

data Enumerator_Generator 
  = Enumerator_Generator {
    enumerator_GeneratorPat :: Pat,
    enumerator_GeneratorRhs :: Data}
  deriving (Eq, Ord, Read, Show)

_Enumerator_Generator = "hydra/ext/scala/meta.Enumerator.Generator"

_Enumerator_Generator_pat = "pat"

_Enumerator_Generator_rhs = "rhs"

data Enumerator_CaseGenerator 
  = Enumerator_CaseGenerator {
    enumerator_CaseGeneratorPat :: Pat,
    enumerator_CaseGeneratorRhs :: Data}
  deriving (Eq, Ord, Read, Show)

_Enumerator_CaseGenerator = "hydra/ext/scala/meta.Enumerator.CaseGenerator"

_Enumerator_CaseGenerator_pat = "pat"

_Enumerator_CaseGenerator_rhs = "rhs"

data Enumerator_Val 
  = Enumerator_Val {
    enumerator_ValPat :: Pat,
    enumerator_ValRhs :: Data}
  deriving (Eq, Ord, Read, Show)

_Enumerator_Val = "hydra/ext/scala/meta.Enumerator.Val"

_Enumerator_Val_pat = "pat"

_Enumerator_Val_rhs = "rhs"

data Enumerator_Guard 
  = Enumerator_Guard {enumerator_GuardCond :: Data}
  deriving (Eq, Ord, Read, Show)

_Enumerator_Guard = "hydra/ext/scala/meta.Enumerator.Guard"

_Enumerator_Guard_cond = "cond"

data ImportExportStat 
  = ImportExportStatImport Import
  | ImportExportStatExport Export
  deriving (Eq, Ord, Read, Show)

_ImportExportStat = "hydra/ext/scala/meta.ImportExportStat"

_ImportExportStat_import = "import"

_ImportExportStat_export = "export"

data Import 
  = Import {importImporters :: [Importer]}
  deriving (Eq, Ord, Read, Show)

_Import = "hydra/ext/scala/meta.Import"

_Import_importers = "importers"

data Export 
  = Export {exportImporters :: [Importer]}
  deriving (Eq, Ord, Read, Show)

_Export = "hydra/ext/scala/meta.Export"

_Export_importers = "importers"

data Importer 
  = Importer {
    importerRef :: Data_Ref,
    importerImportees :: [Importee]}
  deriving (Eq, Ord, Read, Show)

_Importer = "hydra/ext/scala/meta.Importer"

_Importer_ref = "ref"

_Importer_importees = "importees"

data Importee 
  = ImporteeWildcard 
  | ImporteeGiven Importee_Given
  | ImporteeGivenAll 
  | ImporteeName Importee_Name
  | ImporteeRename Importee_Rename
  | ImporteeUnimport Importee_Unimport
  deriving (Eq, Ord, Read, Show)

_Importee = "hydra/ext/scala/meta.Importee"

_Importee_wildcard = "wildcard"

_Importee_given = "given"

_Importee_givenAll = "givenAll"

_Importee_name = "name"

_Importee_rename = "rename"

_Importee_unimport = "unimport"

data Importee_Given 
  = Importee_Given {importee_GivenTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Importee_Given = "hydra/ext/scala/meta.Importee.Given"

_Importee_Given_tpe = "tpe"

data Importee_Name 
  = Importee_Name {importee_NameName :: Name}
  deriving (Eq, Ord, Read, Show)

_Importee_Name = "hydra/ext/scala/meta.Importee.Name"

_Importee_Name_name = "name"

data Importee_Rename 
  = Importee_Rename {
    importee_RenameName :: Name,
    importee_RenameRename :: Name}
  deriving (Eq, Ord, Read, Show)

_Importee_Rename = "hydra/ext/scala/meta.Importee.Rename"

_Importee_Rename_name = "name"

_Importee_Rename_rename = "rename"

data Importee_Unimport 
  = Importee_Unimport {importee_UnimportName :: Name}
  deriving (Eq, Ord, Read, Show)

_Importee_Unimport = "hydra/ext/scala/meta.Importee.Unimport"

_Importee_Unimport_name = "name"

data CaseTree 
  = CaseTreeCase Case
  | CaseTreeTypeCase TypeCase
  deriving (Eq, Ord, Read, Show)

_CaseTree = "hydra/ext/scala/meta.CaseTree"

_CaseTree_case = "case"

_CaseTree_typeCase = "typeCase"

data Case 
  = Case {
    casePat :: Pat,
    caseCond :: (Maybe Data),
    caseBody :: Data}
  deriving (Eq, Ord, Read, Show)

_Case = "hydra/ext/scala/meta.Case"

_Case_pat = "pat"

_Case_cond = "cond"

_Case_body = "body"

data TypeCase 
  = TypeCase {
    typeCasePat :: Type,
    typeCaseBody :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeCase = "hydra/ext/scala/meta.TypeCase"

_TypeCase_pat = "pat"

_TypeCase_body = "body"

data Source 
  = Source {sourceStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)

_Source = "hydra/ext/scala/meta.Source"

_Source_stats = "stats"

data Quasi 
  = Quasi {}
  deriving (Eq, Ord, Read, Show)

_Quasi = "hydra/ext/scala/meta.Quasi"