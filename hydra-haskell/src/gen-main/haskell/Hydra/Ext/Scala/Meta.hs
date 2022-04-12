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
  = StatTerm Term
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


data Term 
  = TermLit Lit
  | TermRef Term_Ref
  | TermInterpolate Term_Interpolate
  | TermXml Term_Xml
  | TermApply Term_Apply
  | TermApplyUsing Term_ApplyUsing
  | TermApplyType Term_ApplyType
  | TermAssign Term_Assign
  | TermReturn Term_Return
  | TermThrow Term_Throw
  | TermAscribe Term_Ascribe
  | TermAnnotate Term_Annotate
  | TermTuple Term_Tuple
  | TermBlock Term_Block
  | TermEndMarker Term_EndMarker
  | TermIf Term_If
  | TermQuotedMacroExpr Term_QuotedMacroExpr
  | TermQuotedMacroType Term_QuotedMacroType
  | TermSplicedMacroExpr Term_SplicedMacroExpr
  | TermMatch Term_Match
  | TermTry Term_Try
  | TermTryWithHandler Term_TryWithHandler
  | TermFunctionTerm Term_FunctionTerm
  | TermPolyFunction Term_PolyFunction
  | TermPartialFunction Term_PartialFunction
  | TermWhile Term_While
  | TermDo Term_Do
  | TermFor Term_For
  | TermForYield Term_ForYield
  | TermNew Term_New
  | TermNewAnonymous Term_NewAnonymous
  | TermPlaceholder Term_Placeholder
  | TermEta Term_Eta
  | TermRepeated Term_Repeated
  | TermParam Term_Param
  deriving (Eq, Ord, Read, Show)

_Term = "hydra/ext/scala/meta.Term"

_Term_lit = "lit"

_Term_ref = "ref"

_Term_interpolate = "interpolate"

_Term_xml = "xml"

_Term_apply = "apply"

_Term_applyUsing = "applyUsing"

_Term_applyType = "applyType"

_Term_assign = "assign"

_Term_return = "return"

_Term_throw = "throw"

_Term_ascribe = "ascribe"

_Term_annotate = "annotate"

_Term_tuple = "tuple"

_Term_block = "block"

_Term_endMarker = "endMarker"

_Term_if = "if"

_Term_quotedMacroExpr = "quotedMacroExpr"

_Term_quotedMacroType = "quotedMacroType"

_Term_splicedMacroExpr = "splicedMacroExpr"

_Term_match = "match"

_Term_try = "try"

_Term_tryWithHandler = "tryWithHandler"

_Term_functionTerm = "functionTerm"

_Term_polyFunction = "polyFunction"

_Term_partialFunction = "partialFunction"

_Term_while = "while"

_Term_do = "do"

_Term_for = "for"

_Term_forYield = "forYield"

_Term_new = "new"

_Term_newAnonymous = "newAnonymous"

_Term_placeholder = "placeholder"

_Term_eta = "eta"

_Term_repeated = "repeated"

_Term_param = "param"


data Term_Ref 
  = Term_RefThis Term_This
  | Term_RefSuper Term_Super
  | Term_RefName Term_Name
  | Term_RefAnonymous Term_Anonymous
  | Term_RefSelect Term_Select
  | Term_RefApplyUnary Term_ApplyUnary
  deriving (Eq, Ord, Read, Show)

_Term_Ref = "hydra/ext/scala/meta.Term.Ref"

_Term_Ref_this = "this"

_Term_Ref_super = "super"

_Term_Ref_name = "name"

_Term_Ref_anonymous = "anonymous"

_Term_Ref_select = "select"

_Term_Ref_applyUnary = "applyUnary"


data Term_This 
  = Term_This {}
  deriving (Eq, Ord, Read, Show)

_Term_This = "hydra/ext/scala/meta.Term.This"


data Term_Super 
  = Term_Super {
    term_SuperThisp :: Name,
    term_SuperSuperp :: Name}
  deriving (Eq, Ord, Read, Show)

_Term_Super = "hydra/ext/scala/meta.Term.Super"

_Term_Super_thisp = "thisp"

_Term_Super_superp = "superp"


data Term_Name 
  = Term_Name {term_NameValue :: PredefString}
  deriving (Eq, Ord, Read, Show)

_Term_Name = "hydra/ext/scala/meta.Term.Name"

_Term_Name_value = "value"


data Term_Anonymous 
  = Term_Anonymous {}
  deriving (Eq, Ord, Read, Show)

_Term_Anonymous = "hydra/ext/scala/meta.Term.Anonymous"


data Term_Select 
  = Term_Select {
    term_SelectQual :: Term,
    term_SelectName :: Term_Name}
  deriving (Eq, Ord, Read, Show)

_Term_Select = "hydra/ext/scala/meta.Term.Select"

_Term_Select_qual = "qual"

_Term_Select_name = "name"


data Term_Interpolate 
  = Term_Interpolate {
    term_InterpolatePrefix :: Term_Name,
    term_InterpolateParts :: [Lit],
    term_InterpolateArgs :: [Term]}
  deriving (Eq, Ord, Read, Show)

_Term_Interpolate = "hydra/ext/scala/meta.Term.Interpolate"

_Term_Interpolate_prefix = "prefix"

_Term_Interpolate_parts = "parts"

_Term_Interpolate_args = "args"


data Term_Xml 
  = Term_Xml {
    term_XmlParts :: [Lit],
    term_XmlArgs :: [Term]}
  deriving (Eq, Ord, Read, Show)

_Term_Xml = "hydra/ext/scala/meta.Term.Xml"

_Term_Xml_parts = "parts"

_Term_Xml_args = "args"


data Term_Apply 
  = Term_Apply {
    term_ApplyFun :: Term,
    term_ApplyArgs :: [Term]}
  deriving (Eq, Ord, Read, Show)

_Term_Apply = "hydra/ext/scala/meta.Term.Apply"

_Term_Apply_fun = "fun"

_Term_Apply_args = "args"


data Term_ApplyUsing 
  = Term_ApplyUsing {
    term_ApplyUsingFun :: Term,
    term_ApplyUsingTargs :: [Term]}
  deriving (Eq, Ord, Read, Show)

_Term_ApplyUsing = "hydra/ext/scala/meta.Term.ApplyUsing"

_Term_ApplyUsing_fun = "fun"

_Term_ApplyUsing_targs = "targs"


data Term_ApplyType 
  = Term_ApplyType {
    term_ApplyTypeLhs :: Term,
    term_ApplyTypeOp :: Term_Name,
    term_ApplyTypeTargs :: [Type],
    term_ApplyTypeArgs :: [Term]}
  deriving (Eq, Ord, Read, Show)

_Term_ApplyType = "hydra/ext/scala/meta.Term.ApplyType"

_Term_ApplyType_lhs = "lhs"

_Term_ApplyType_op = "op"

_Term_ApplyType_targs = "targs"

_Term_ApplyType_args = "args"


data Term_ApplyInfix 
  = Term_ApplyInfix {
    term_ApplyInfixLhs :: Term,
    term_ApplyInfixOp :: Term_Name,
    term_ApplyInfixTargs :: [Type],
    term_ApplyInfixArgs :: [Term]}
  deriving (Eq, Ord, Read, Show)

_Term_ApplyInfix = "hydra/ext/scala/meta.Term.ApplyInfix"

_Term_ApplyInfix_lhs = "lhs"

_Term_ApplyInfix_op = "op"

_Term_ApplyInfix_targs = "targs"

_Term_ApplyInfix_args = "args"


data Term_ApplyUnary 
  = Term_ApplyUnary {
    term_ApplyUnaryOp :: Term_Name,
    term_ApplyUnaryArg :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_ApplyUnary = "hydra/ext/scala/meta.Term.ApplyUnary"

_Term_ApplyUnary_op = "op"

_Term_ApplyUnary_arg = "arg"


data Term_Assign 
  = Term_Assign {
    term_AssignLhs :: Term,
    term_AssignRhs :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_Assign = "hydra/ext/scala/meta.Term.Assign"

_Term_Assign_lhs = "lhs"

_Term_Assign_rhs = "rhs"


data Term_Return 
  = Term_Return {term_ReturnExpr :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_Return = "hydra/ext/scala/meta.Term.Return"

_Term_Return_expr = "expr"


data Term_Throw 
  = Term_Throw {term_ThrowExpr :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_Throw = "hydra/ext/scala/meta.Term.Throw"

_Term_Throw_expr = "expr"


data Term_Ascribe 
  = Term_Ascribe {
    term_AscribeExpr :: Term,
    term_AscribeTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Term_Ascribe = "hydra/ext/scala/meta.Term.Ascribe"

_Term_Ascribe_expr = "expr"

_Term_Ascribe_tpe = "tpe"


data Term_Annotate 
  = Term_Annotate {
    term_AnnotateExpr :: Term,
    term_AnnotateAnnots :: [Mod_Annot]}
  deriving (Eq, Ord, Read, Show)

_Term_Annotate = "hydra/ext/scala/meta.Term.Annotate"

_Term_Annotate_expr = "expr"

_Term_Annotate_annots = "annots"


data Term_Tuple 
  = Term_Tuple {term_TupleArgs :: [Term]}
  deriving (Eq, Ord, Read, Show)

_Term_Tuple = "hydra/ext/scala/meta.Term.Tuple"

_Term_Tuple_args = "args"


data Term_Block 
  = Term_Block {term_BlockStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)

_Term_Block = "hydra/ext/scala/meta.Term.Block"

_Term_Block_stats = "stats"


data Term_EndMarker 
  = Term_EndMarker {term_EndMarkerName :: Term_Name}
  deriving (Eq, Ord, Read, Show)

_Term_EndMarker = "hydra/ext/scala/meta.Term.EndMarker"

_Term_EndMarker_name = "name"


data Term_If 
  = Term_If {
    term_IfCond :: Term,
    term_IfThenp :: Term,
    term_IfElsep :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_If = "hydra/ext/scala/meta.Term.If"

_Term_If_cond = "cond"

_Term_If_thenp = "thenp"

_Term_If_elsep = "elsep"


data Term_QuotedMacroExpr 
  = Term_QuotedMacroExpr {term_QuotedMacroExprBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_QuotedMacroExpr = "hydra/ext/scala/meta.Term.QuotedMacroExpr"

_Term_QuotedMacroExpr_body = "body"


data Term_QuotedMacroType 
  = Term_QuotedMacroType {term_QuotedMacroTypeTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Term_QuotedMacroType = "hydra/ext/scala/meta.Term.QuotedMacroType"

_Term_QuotedMacroType_tpe = "tpe"


data Term_SplicedMacroExpr 
  = Term_SplicedMacroExpr {term_SplicedMacroExprBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_SplicedMacroExpr = "hydra/ext/scala/meta.Term.SplicedMacroExpr"

_Term_SplicedMacroExpr_body = "body"


data Term_Match 
  = Term_Match {
    term_MatchExpr :: Term,
    term_MatchCases :: [Case]}
  deriving (Eq, Ord, Read, Show)

_Term_Match = "hydra/ext/scala/meta.Term.Match"

_Term_Match_expr = "expr"

_Term_Match_cases = "cases"


data Term_Try 
  = Term_Try {
    term_TryExpr :: Term,
    term_TryCatchp :: [Case],
    term_TryFinallyp :: (Maybe Term)}
  deriving (Eq, Ord, Read, Show)

_Term_Try = "hydra/ext/scala/meta.Term.Try"

_Term_Try_expr = "expr"

_Term_Try_catchp = "catchp"

_Term_Try_finallyp = "finallyp"


data Term_TryWithHandler 
  = Term_TryWithHandler {
    term_TryWithHandlerExpr :: Term,
    term_TryWithHandlerCatchp :: Term,
    term_TryWithHandlerFinallyp :: (Maybe Term)}
  deriving (Eq, Ord, Read, Show)

_Term_TryWithHandler = "hydra/ext/scala/meta.Term.TryWithHandler"

_Term_TryWithHandler_expr = "expr"

_Term_TryWithHandler_catchp = "catchp"

_Term_TryWithHandler_finallyp = "finallyp"


data Term_FunctionTerm 
  = Term_FunctionTermContextFunction Term_ContextFunction
  | Term_FunctionTermFunction Term_Function
  deriving (Eq, Ord, Read, Show)

_Term_FunctionTerm = "hydra/ext/scala/meta.Term.FunctionTerm"

_Term_FunctionTerm_contextFunction = "contextFunction"

_Term_FunctionTerm_function = "function"


data Term_ContextFunction 
  = Term_ContextFunction {
    term_ContextFunctionParams :: [Term_Param],
    term_ContextFunctionBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_ContextFunction = "hydra/ext/scala/meta.Term.ContextFunction"

_Term_ContextFunction_params = "params"

_Term_ContextFunction_body = "body"


data Term_Function 
  = Term_Function {
    term_FunctionParams :: [Term_Param],
    term_FunctionBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_Function = "hydra/ext/scala/meta.Term.Function"

_Term_Function_params = "params"

_Term_Function_body = "body"


data Term_PolyFunction 
  = Term_PolyFunction {
    term_PolyFunctionTparams :: [Type_Param],
    term_PolyFunctionBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_PolyFunction = "hydra/ext/scala/meta.Term.PolyFunction"

_Term_PolyFunction_tparams = "tparams"

_Term_PolyFunction_body = "body"


data Term_PartialFunction 
  = Term_PartialFunction {term_PartialFunctionCases :: [Case]}
  deriving (Eq, Ord, Read, Show)

_Term_PartialFunction = "hydra/ext/scala/meta.Term.PartialFunction"

_Term_PartialFunction_cases = "cases"


data Term_While 
  = Term_While {
    term_WhileExpr :: Term,
    term_WhileBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_While = "hydra/ext/scala/meta.Term.While"

_Term_While_expr = "expr"

_Term_While_body = "body"


data Term_Do 
  = Term_Do {
    term_DoBody :: Term,
    term_DoExpr :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_Do = "hydra/ext/scala/meta.Term.Do"

_Term_Do_body = "body"

_Term_Do_expr = "expr"


data Term_For 
  = Term_For {term_ForEnums :: [Enumerator]}
  deriving (Eq, Ord, Read, Show)

_Term_For = "hydra/ext/scala/meta.Term.For"

_Term_For_enums = "enums"


data Term_ForYield 
  = Term_ForYield {term_ForYieldEnums :: [Enumerator]}
  deriving (Eq, Ord, Read, Show)

_Term_ForYield = "hydra/ext/scala/meta.Term.ForYield"

_Term_ForYield_enums = "enums"


data Term_New 
  = Term_New {term_NewInit :: Init}
  deriving (Eq, Ord, Read, Show)

_Term_New = "hydra/ext/scala/meta.Term.New"

_Term_New_init = "init"


data Term_NewAnonymous 
  = Term_NewAnonymous {term_NewAnonymousTempl :: Template}
  deriving (Eq, Ord, Read, Show)

_Term_NewAnonymous = "hydra/ext/scala/meta.Term.NewAnonymous"

_Term_NewAnonymous_templ = "templ"


data Term_Placeholder 
  = Term_Placeholder {}
  deriving (Eq, Ord, Read, Show)

_Term_Placeholder = "hydra/ext/scala/meta.Term.Placeholder"


data Term_Eta 
  = Term_Eta {term_EtaExpr :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_Eta = "hydra/ext/scala/meta.Term.Eta"

_Term_Eta_expr = "expr"


data Term_Repeated 
  = Term_Repeated {term_RepeatedExpr :: Term}
  deriving (Eq, Ord, Read, Show)

_Term_Repeated = "hydra/ext/scala/meta.Term.Repeated"

_Term_Repeated_expr = "expr"


data Term_Param 
  = Term_Param {
    term_ParamMods :: [Mod],
    term_ParamName :: Name,
    term_ParamDecltpe :: (Maybe Type),
    term_ParamDefault :: (Maybe Term)}
  deriving (Eq, Ord, Read, Show)

_Term_Param = "hydra/ext/scala/meta.Term.Param"

_Term_Param_mods = "mods"

_Term_Param_name = "name"

_Term_Param_decltpe = "decltpe"

_Term_Param_default = "default"


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
    type_SelectQual :: Term_Ref,
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
  = Type_Singleton {type_SingletonRef :: Term_Ref}
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
  = Type_Macro {type_MacroBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Type_Macro = "hydra/ext/scala/meta.Type.Macro"

_Type_Macro_body = "body"


data Type_Method 
  = Type_Method {
    type_MethodParamss :: [[Term_Param]],
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
  = Pat_Var {pat_VarName :: Term_Name}
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
  = Pat_Repeated {pat_RepeatedName :: Term_Name}
  deriving (Eq, Ord, Read, Show)

_Pat_Repeated = "hydra/ext/scala/meta.Pat.Repeated"

_Pat_Repeated_name = "name"


data Pat_Extract 
  = Pat_Extract {
    pat_ExtractFun :: Term,
    pat_ExtractArgs :: [Pat]}
  deriving (Eq, Ord, Read, Show)

_Pat_Extract = "hydra/ext/scala/meta.Pat.Extract"

_Pat_Extract_fun = "fun"

_Pat_Extract_args = "args"


data Pat_ExtractInfix 
  = Pat_ExtractInfix {
    pat_ExtractInfixLhs :: Pat,
    pat_ExtractInfixOp :: Term_Name,
    pat_ExtractInfixRhs :: [Pat]}
  deriving (Eq, Ord, Read, Show)

_Pat_ExtractInfix = "hydra/ext/scala/meta.Pat.ExtractInfix"

_Pat_ExtractInfix_lhs = "lhs"

_Pat_ExtractInfix_op = "op"

_Pat_ExtractInfix_rhs = "rhs"


data Pat_Interpolate 
  = Pat_Interpolate {
    pat_InterpolatePrefix :: Term_Name,
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
  = Pat_Macro {pat_MacroBody :: Term}
  deriving (Eq, Ord, Read, Show)

_Pat_Macro = "hydra/ext/scala/meta.Pat.Macro"

_Pat_Macro_body = "body"


data Pat_Given 
  = Pat_Given {pat_GivenTpe :: Type}
  deriving (Eq, Ord, Read, Show)

_Pat_Given = "hydra/ext/scala/meta.Pat.Given"

_Pat_Given_tpe = "tpe"


data Member 
  = MemberTerm Member_Term
  | MemberType Member_Type
  | MemberTermParam Term_Param
  | MemberTypeParam Type_Param
  | MemberSelf Self
  deriving (Eq, Ord, Read, Show)

_Member = "hydra/ext/scala/meta.Member"

_Member_term = "term"

_Member_type = "type"

_Member_termParam = "termParam"

_Member_typeParam = "typeParam"

_Member_self = "self"


data Member_Term 
  = Member_TermPkg Pkg
  | Member_TermObject Pkg_Object
  deriving (Eq, Ord, Read, Show)

_Member_Term = "hydra/ext/scala/meta.Member.Term"

_Member_Term_pkg = "pkg"

_Member_Term_object = "object"


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
    decl_DefName :: Term_Name,
    decl_DefTparams :: [Type_Param],
    decl_DefParamss :: [[Term_Param]],
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
    decl_GivenName :: Term_Name,
    decl_GivenTparams :: [Type_Param],
    decl_GivenSparams :: [[Term_Param]],
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
    defn_ValRhs :: Term}
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
    defn_VarRhs :: (Maybe Term)}
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
    defn_GivenSparams :: [[Term_Param]],
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
    defn_EnumCaseName :: Term_Name,
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
    defn_RepeatedEnumCaseCases :: [Term_Name]}
  deriving (Eq, Ord, Read, Show)

_Defn_RepeatedEnumCase = "hydra/ext/scala/meta.Defn.RepeatedEnumCase"

_Defn_RepeatedEnumCase_mods = "mods"

_Defn_RepeatedEnumCase_cases = "cases"


data Defn_GivenAlias 
  = Defn_GivenAlias {
    defn_GivenAliasMods :: [Mod],
    defn_GivenAliasName :: Name,
    defn_GivenAliasTparams :: [[Type_Param]],
    defn_GivenAliasSparams :: [[Term_Param]],
    defn_GivenAliasDecltpe :: Type,
    defn_GivenAliasBody :: Term}
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
    defn_ExtensionGroupParmss :: [[Term_Param]],
    defn_ExtensionGroupBody :: Stat}
  deriving (Eq, Ord, Read, Show)

_Defn_ExtensionGroup = "hydra/ext/scala/meta.Defn.ExtensionGroup"

_Defn_ExtensionGroup_tparams = "tparams"

_Defn_ExtensionGroup_parmss = "parmss"

_Defn_ExtensionGroup_body = "body"


data Defn_Def 
  = Defn_Def {
    defn_DefMods :: [Mod],
    defn_DefName :: Term_Name,
    defn_DefTparams :: [Type_Param],
    defn_DefParamss :: [[Term_Param]],
    defn_DefDecltpe :: (Maybe Type),
    defn_DefBody :: Term}
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
    defn_MacroName :: Term_Name,
    defn_MacroTparams :: [Type_Param],
    defn_MacroParamss :: [[Term_Param]],
    defn_MacroDecltpe :: (Maybe Type),
    defn_MacroBody :: Term}
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
  = Defn_Object {defn_ObjectName :: Term_Name}
  deriving (Eq, Ord, Read, Show)

_Defn_Object = "hydra/ext/scala/meta.Defn.Object"

_Defn_Object_name = "name"


data Pkg 
  = Pkg {
    pkgName :: Term_Name,
    pkgRef :: Term_Ref,
    pkgStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)

_Pkg = "hydra/ext/scala/meta.Pkg"

_Pkg_name = "name"

_Pkg_ref = "ref"

_Pkg_stats = "stats"


data Pkg_Object 
  = Pkg_Object {
    pkg_ObjectMods :: [Mod],
    pkg_ObjectName :: Term_Name,
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
    ctor_PrimaryParamss :: [[Term_Param]]}
  deriving (Eq, Ord, Read, Show)

_Ctor_Primary = "hydra/ext/scala/meta.Ctor.Primary"

_Ctor_Primary_mods = "mods"

_Ctor_Primary_name = "name"

_Ctor_Primary_paramss = "paramss"


data Ctor_Secondary 
  = Ctor_Secondary {
    ctor_SecondaryMods :: [Mod],
    ctor_SecondaryName :: Name,
    ctor_SecondaryParamss :: [[Term_Param]],
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
    initArgss :: [[Term]]}
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
    enumerator_GeneratorRhs :: Term}
  deriving (Eq, Ord, Read, Show)

_Enumerator_Generator = "hydra/ext/scala/meta.Enumerator.Generator"

_Enumerator_Generator_pat = "pat"

_Enumerator_Generator_rhs = "rhs"


data Enumerator_CaseGenerator 
  = Enumerator_CaseGenerator {
    enumerator_CaseGeneratorPat :: Pat,
    enumerator_CaseGeneratorRhs :: Term}
  deriving (Eq, Ord, Read, Show)

_Enumerator_CaseGenerator = "hydra/ext/scala/meta.Enumerator.CaseGenerator"

_Enumerator_CaseGenerator_pat = "pat"

_Enumerator_CaseGenerator_rhs = "rhs"


data Enumerator_Val 
  = Enumerator_Val {
    enumerator_ValPat :: Pat,
    enumerator_ValRhs :: Term}
  deriving (Eq, Ord, Read, Show)

_Enumerator_Val = "hydra/ext/scala/meta.Enumerator.Val"

_Enumerator_Val_pat = "pat"

_Enumerator_Val_rhs = "rhs"


data Enumerator_Guard 
  = Enumerator_Guard {enumerator_GuardCond :: Term}
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
    importerRef :: Term_Ref,
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
    caseCond :: (Maybe Term),
    caseBody :: Term}
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