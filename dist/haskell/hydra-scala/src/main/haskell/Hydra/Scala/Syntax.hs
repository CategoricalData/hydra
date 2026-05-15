-- Note: this is an automatically generated file. Do not edit.
-- | A Scala syntax model based on Scalameta (https://scalameta.org)

module Hydra.Scala.Syntax where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I
newtype PredefString =
  PredefString {
    unPredefString :: String}
  deriving (Eq, Ord, Read, Show)
_PredefString = Core.Name "hydra.scala.syntax.PredefString"
data ScalaSymbol =
  ScalaSymbol {
    scalaSymbolName :: String}
  deriving (Eq, Ord, Read, Show)
_ScalaSymbol = Core.Name "hydra.scala.syntax.ScalaSymbol"
_ScalaSymbol_name = Core.Name "name"
data Tree =
  TreeRef Ref |
  TreeStat Stat |
  TreeType Type |
  TreeBounds TypeBounds |
  TreePat Pat |
  TreeMember Member |
  TreeCtor Ctor |
  TreeTemplate Template |
  TreeMod Mod |
  TreeEnumerator Enumerator |
  TreeImporter Importer |
  TreeImportee Importee |
  TreeCaseTree CaseTree |
  TreeSource Source
  deriving (Eq, Ord, Read, Show)
_Tree = Core.Name "hydra.scala.syntax.Tree"
_Tree_ref = Core.Name "ref"
_Tree_stat = Core.Name "stat"
_Tree_type = Core.Name "type"
_Tree_bounds = Core.Name "bounds"
_Tree_pat = Core.Name "pat"
_Tree_member = Core.Name "member"
_Tree_ctor = Core.Name "ctor"
_Tree_template = Core.Name "template"
_Tree_mod = Core.Name "mod"
_Tree_enumerator = Core.Name "enumerator"
_Tree_importer = Core.Name "importer"
_Tree_importee = Core.Name "importee"
_Tree_caseTree = Core.Name "caseTree"
_Tree_source = Core.Name "source"
data Ref =
  RefName Name |
  RefInit Init
  deriving (Eq, Ord, Read, Show)
_Ref = Core.Name "hydra.scala.syntax.Ref"
_Ref_name = Core.Name "name"
_Ref_init = Core.Name "init"
data Stat =
  StatTerm Data |
  StatDecl Decl |
  StatDefn Defn |
  StatImportExport ImportExportStat
  deriving (Eq, Ord, Read, Show)
_Stat = Core.Name "hydra.scala.syntax.Stat"
_Stat_term = Core.Name "term"
_Stat_decl = Core.Name "decl"
_Stat_defn = Core.Name "defn"
_Stat_importExport = Core.Name "importExport"
data Name =
  NameValue String |
  NameAnonymous |
  NameIndeterminate PredefString
  deriving (Eq, Ord, Read, Show)
_Name = Core.Name "hydra.scala.syntax.Name"
_Name_value = Core.Name "value"
_Name_anonymous = Core.Name "anonymous"
_Name_indeterminate = Core.Name "indeterminate"
data Lit =
  LitNull |
  LitInt Int |
  LitDouble Double |
  LitFloat Float |
  LitByte I.Int8 |
  LitShort I.Int16 |
  LitChar Int |
  LitLong I.Int64 |
  LitBoolean Bool |
  LitUnit |
  LitString String |
  LitBytes [Int] |
  LitSymbol ScalaSymbol
  deriving (Eq, Ord, Read, Show)
_Lit = Core.Name "hydra.scala.syntax.Lit"
_Lit_null = Core.Name "null"
_Lit_int = Core.Name "int"
_Lit_double = Core.Name "double"
_Lit_float = Core.Name "float"
_Lit_byte = Core.Name "byte"
_Lit_short = Core.Name "short"
_Lit_char = Core.Name "char"
_Lit_long = Core.Name "long"
_Lit_boolean = Core.Name "boolean"
_Lit_unit = Core.Name "unit"
_Lit_string = Core.Name "string"
_Lit_bytes = Core.Name "bytes"
_Lit_symbol = Core.Name "symbol"
data Data =
  DataLit Lit |
  DataRef RefData |
  DataInterpolate InterpolateData |
  DataApply ApplyData |
  DataApplyUsing ApplyUsingData |
  DataApplyType ApplyTypeData |
  DataAssign AssignData |
  DataReturn ReturnData |
  DataThrow ThrowData |
  DataAscribe AscribeData |
  DataAnnotate AnnotateData |
  DataTuple TupleData |
  DataBlock BlockData |
  DataEndMarker EndMarkerData |
  DataIf IfData |
  DataMatch MatchData |
  DataTry TryData |
  DataTryWithHandler TryWithHandlerData |
  DataFunction FunctionData |
  DataContextFunction ContextFunctionData |
  DataPolyFunction PolyFunctionData |
  DataPartialFunction PartialFunctionData |
  DataWhile WhileData |
  DataDo DoData |
  DataFor ForData |
  DataForYield ForYieldData |
  DataNew NewData |
  DataNewAnonymous NewAnonymousData |
  DataPlaceholder |
  DataEta EtaData |
  DataRepeated RepeatedData |
  DataParam ParamData
  deriving (Eq, Ord, Read, Show)
_Data = Core.Name "hydra.scala.syntax.Data"
_Data_lit = Core.Name "lit"
_Data_ref = Core.Name "ref"
_Data_interpolate = Core.Name "interpolate"
_Data_apply = Core.Name "apply"
_Data_applyUsing = Core.Name "applyUsing"
_Data_applyType = Core.Name "applyType"
_Data_assign = Core.Name "assign"
_Data_return = Core.Name "return"
_Data_throw = Core.Name "throw"
_Data_ascribe = Core.Name "ascribe"
_Data_annotate = Core.Name "annotate"
_Data_tuple = Core.Name "tuple"
_Data_block = Core.Name "block"
_Data_endMarker = Core.Name "endMarker"
_Data_if = Core.Name "if"
_Data_match = Core.Name "match"
_Data_try = Core.Name "try"
_Data_tryWithHandler = Core.Name "tryWithHandler"
_Data_function = Core.Name "function"
_Data_contextFunction = Core.Name "contextFunction"
_Data_polyFunction = Core.Name "polyFunction"
_Data_partialFunction = Core.Name "partialFunction"
_Data_while = Core.Name "while"
_Data_do = Core.Name "do"
_Data_for = Core.Name "for"
_Data_forYield = Core.Name "forYield"
_Data_new = Core.Name "new"
_Data_newAnonymous = Core.Name "newAnonymous"
_Data_placeholder = Core.Name "placeholder"
_Data_eta = Core.Name "eta"
_Data_repeated = Core.Name "repeated"
_Data_param = Core.Name "param"
data RefData =
  RefDataThis ThisData |
  RefDataSuper SuperData |
  RefDataName NameData |
  RefDataAnonymous AnonymousData |
  RefDataSelect SelectData |
  RefDataApplyUnary ApplyUnaryData
  deriving (Eq, Ord, Read, Show)
_RefData = Core.Name "hydra.scala.syntax.RefData"
_RefData_this = Core.Name "this"
_RefData_super = Core.Name "super"
_RefData_name = Core.Name "name"
_RefData_anonymous = Core.Name "anonymous"
_RefData_select = Core.Name "select"
_RefData_applyUnary = Core.Name "applyUnary"
newtype ThisData =
  ThisData {
    unThisData :: ()}
  deriving (Eq, Ord, Read, Show)
_ThisData = Core.Name "hydra.scala.syntax.ThisData"
data SuperData =
  SuperData {
    superDataThisp :: Name,
    superDataSuperp :: Name}
  deriving (Eq, Ord, Read, Show)
_SuperData = Core.Name "hydra.scala.syntax.SuperData"
_SuperData_thisp = Core.Name "thisp"
_SuperData_superp = Core.Name "superp"
data NameData =
  NameData {
    nameDataValue :: PredefString}
  deriving (Eq, Ord, Read, Show)
_NameData = Core.Name "hydra.scala.syntax.NameData"
_NameData_value = Core.Name "value"
newtype AnonymousData =
  AnonymousData {
    unAnonymousData :: ()}
  deriving (Eq, Ord, Read, Show)
_AnonymousData = Core.Name "hydra.scala.syntax.AnonymousData"
data SelectData =
  SelectData {
    selectDataQual :: Data,
    selectDataName :: NameData}
  deriving (Eq, Ord, Read, Show)
_SelectData = Core.Name "hydra.scala.syntax.SelectData"
_SelectData_qual = Core.Name "qual"
_SelectData_name = Core.Name "name"
data InterpolateData =
  InterpolateData {
    interpolateDataPrefix :: NameData,
    interpolateDataParts :: [Lit],
    interpolateDataArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)
_InterpolateData = Core.Name "hydra.scala.syntax.InterpolateData"
_InterpolateData_prefix = Core.Name "prefix"
_InterpolateData_parts = Core.Name "parts"
_InterpolateData_args = Core.Name "args"
data ApplyData =
  ApplyData {
    applyDataFun :: Data,
    applyDataArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)
_ApplyData = Core.Name "hydra.scala.syntax.ApplyData"
_ApplyData_fun = Core.Name "fun"
_ApplyData_args = Core.Name "args"
data ApplyUsingData =
  ApplyUsingData {
    applyUsingDataFun :: Data,
    applyUsingDataTargs :: [Data]}
  deriving (Eq, Ord, Read, Show)
_ApplyUsingData = Core.Name "hydra.scala.syntax.ApplyUsingData"
_ApplyUsingData_fun = Core.Name "fun"
_ApplyUsingData_targs = Core.Name "targs"
data ApplyTypeData =
  ApplyTypeData {
    applyTypeDataLhs :: Data,
    applyTypeDataOp :: NameData,
    applyTypeDataTargs :: [Type],
    applyTypeDataArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)
_ApplyTypeData = Core.Name "hydra.scala.syntax.ApplyTypeData"
_ApplyTypeData_lhs = Core.Name "lhs"
_ApplyTypeData_op = Core.Name "op"
_ApplyTypeData_targs = Core.Name "targs"
_ApplyTypeData_args = Core.Name "args"
data ApplyInfixData =
  ApplyInfixData {
    applyInfixDataLhs :: Data,
    applyInfixDataOp :: NameData,
    applyInfixDataTargs :: [Type],
    applyInfixDataArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)
_ApplyInfixData = Core.Name "hydra.scala.syntax.ApplyInfixData"
_ApplyInfixData_lhs = Core.Name "lhs"
_ApplyInfixData_op = Core.Name "op"
_ApplyInfixData_targs = Core.Name "targs"
_ApplyInfixData_args = Core.Name "args"
data ApplyUnaryData =
  ApplyUnaryData {
    applyUnaryDataOp :: NameData,
    applyUnaryDataArg :: Data}
  deriving (Eq, Ord, Read, Show)
_ApplyUnaryData = Core.Name "hydra.scala.syntax.ApplyUnaryData"
_ApplyUnaryData_op = Core.Name "op"
_ApplyUnaryData_arg = Core.Name "arg"
data AssignData =
  AssignData {
    assignDataLhs :: Data,
    assignDataRhs :: Data}
  deriving (Eq, Ord, Read, Show)
_AssignData = Core.Name "hydra.scala.syntax.AssignData"
_AssignData_lhs = Core.Name "lhs"
_AssignData_rhs = Core.Name "rhs"
data ReturnData =
  ReturnData {
    returnDataExpr :: Data}
  deriving (Eq, Ord, Read, Show)
_ReturnData = Core.Name "hydra.scala.syntax.ReturnData"
_ReturnData_expr = Core.Name "expr"
data ThrowData =
  ThrowData {
    throwDataExpr :: Data}
  deriving (Eq, Ord, Read, Show)
_ThrowData = Core.Name "hydra.scala.syntax.ThrowData"
_ThrowData_expr = Core.Name "expr"
data AscribeData =
  AscribeData {
    ascribeDataExpr :: Data,
    ascribeDataTpe :: Type}
  deriving (Eq, Ord, Read, Show)
_AscribeData = Core.Name "hydra.scala.syntax.AscribeData"
_AscribeData_expr = Core.Name "expr"
_AscribeData_tpe = Core.Name "tpe"
data AnnotateData =
  AnnotateData {
    annotateDataExpr :: Data,
    annotateDataAnnots :: [AnnotMod]}
  deriving (Eq, Ord, Read, Show)
_AnnotateData = Core.Name "hydra.scala.syntax.AnnotateData"
_AnnotateData_expr = Core.Name "expr"
_AnnotateData_annots = Core.Name "annots"
data TupleData =
  TupleData {
    tupleDataArgs :: [Data]}
  deriving (Eq, Ord, Read, Show)
_TupleData = Core.Name "hydra.scala.syntax.TupleData"
_TupleData_args = Core.Name "args"
data BlockData =
  BlockData {
    blockDataStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)
_BlockData = Core.Name "hydra.scala.syntax.BlockData"
_BlockData_stats = Core.Name "stats"
data EndMarkerData =
  EndMarkerData {
    endMarkerDataName :: NameData}
  deriving (Eq, Ord, Read, Show)
_EndMarkerData = Core.Name "hydra.scala.syntax.EndMarkerData"
_EndMarkerData_name = Core.Name "name"
data IfData =
  IfData {
    ifDataCond :: Data,
    ifDataThenp :: Data,
    ifDataElsep :: Data}
  deriving (Eq, Ord, Read, Show)
_IfData = Core.Name "hydra.scala.syntax.IfData"
_IfData_cond = Core.Name "cond"
_IfData_thenp = Core.Name "thenp"
_IfData_elsep = Core.Name "elsep"
data MatchData =
  MatchData {
    matchDataExpr :: Data,
    matchDataCases :: [Case]}
  deriving (Eq, Ord, Read, Show)
_MatchData = Core.Name "hydra.scala.syntax.MatchData"
_MatchData_expr = Core.Name "expr"
_MatchData_cases = Core.Name "cases"
data TryData =
  TryData {
    tryDataExpr :: Data,
    tryDataCatchp :: [Case],
    tryDataFinallyp :: (Maybe Data)}
  deriving (Eq, Ord, Read, Show)
_TryData = Core.Name "hydra.scala.syntax.TryData"
_TryData_expr = Core.Name "expr"
_TryData_catchp = Core.Name "catchp"
_TryData_finallyp = Core.Name "finallyp"
data TryWithHandlerData =
  TryWithHandlerData {
    tryWithHandlerDataExpr :: Data,
    tryWithHandlerDataCatchp :: Data,
    tryWithHandlerDataFinallyp :: (Maybe Data)}
  deriving (Eq, Ord, Read, Show)
_TryWithHandlerData = Core.Name "hydra.scala.syntax.TryWithHandlerData"
_TryWithHandlerData_expr = Core.Name "expr"
_TryWithHandlerData_catchp = Core.Name "catchp"
_TryWithHandlerData_finallyp = Core.Name "finallyp"
data ContextFunctionData =
  ContextFunctionData {
    contextFunctionDataParams :: [ParamData],
    contextFunctionDataBody :: Data}
  deriving (Eq, Ord, Read, Show)
_ContextFunctionData = Core.Name "hydra.scala.syntax.ContextFunctionData"
_ContextFunctionData_params = Core.Name "params"
_ContextFunctionData_body = Core.Name "body"
data FunctionData =
  FunctionData {
    functionDataParams :: [ParamData],
    functionDataBody :: Data}
  deriving (Eq, Ord, Read, Show)
_FunctionData = Core.Name "hydra.scala.syntax.FunctionData"
_FunctionData_params = Core.Name "params"
_FunctionData_body = Core.Name "body"
data PolyFunctionData =
  PolyFunctionData {
    polyFunctionDataTparams :: [ParamType],
    polyFunctionDataBody :: Data}
  deriving (Eq, Ord, Read, Show)
_PolyFunctionData = Core.Name "hydra.scala.syntax.PolyFunctionData"
_PolyFunctionData_tparams = Core.Name "tparams"
_PolyFunctionData_body = Core.Name "body"
data PartialFunctionData =
  PartialFunctionData {
    partialFunctionDataCases :: [Case]}
  deriving (Eq, Ord, Read, Show)
_PartialFunctionData = Core.Name "hydra.scala.syntax.PartialFunctionData"
_PartialFunctionData_cases = Core.Name "cases"
data WhileData =
  WhileData {
    whileDataExpr :: Data,
    whileDataBody :: Data}
  deriving (Eq, Ord, Read, Show)
_WhileData = Core.Name "hydra.scala.syntax.WhileData"
_WhileData_expr = Core.Name "expr"
_WhileData_body = Core.Name "body"
data DoData =
  DoData {
    doDataBody :: Data,
    doDataExpr :: Data}
  deriving (Eq, Ord, Read, Show)
_DoData = Core.Name "hydra.scala.syntax.DoData"
_DoData_body = Core.Name "body"
_DoData_expr = Core.Name "expr"
data ForData =
  ForData {
    forDataEnums :: [Enumerator]}
  deriving (Eq, Ord, Read, Show)
_ForData = Core.Name "hydra.scala.syntax.ForData"
_ForData_enums = Core.Name "enums"
data ForYieldData =
  ForYieldData {
    forYieldDataEnums :: [Enumerator]}
  deriving (Eq, Ord, Read, Show)
_ForYieldData = Core.Name "hydra.scala.syntax.ForYieldData"
_ForYieldData_enums = Core.Name "enums"
data NewData =
  NewData {
    newDataInit :: Init}
  deriving (Eq, Ord, Read, Show)
_NewData = Core.Name "hydra.scala.syntax.NewData"
_NewData_init = Core.Name "init"
data NewAnonymousData =
  NewAnonymousData {
    newAnonymousDataTempl :: Template}
  deriving (Eq, Ord, Read, Show)
_NewAnonymousData = Core.Name "hydra.scala.syntax.NewAnonymousData"
_NewAnonymousData_templ = Core.Name "templ"
data EtaData =
  EtaData {
    etaDataExpr :: Data}
  deriving (Eq, Ord, Read, Show)
_EtaData = Core.Name "hydra.scala.syntax.EtaData"
_EtaData_expr = Core.Name "expr"
data RepeatedData =
  RepeatedData {
    repeatedDataExpr :: Data}
  deriving (Eq, Ord, Read, Show)
_RepeatedData = Core.Name "hydra.scala.syntax.RepeatedData"
_RepeatedData_expr = Core.Name "expr"
data ParamData =
  ParamData {
    paramDataMods :: [Mod],
    paramDataName :: Name,
    paramDataDecltpe :: (Maybe Type),
    paramDataDefault :: (Maybe Data)}
  deriving (Eq, Ord, Read, Show)
_ParamData = Core.Name "hydra.scala.syntax.ParamData"
_ParamData_mods = Core.Name "mods"
_ParamData_name = Core.Name "name"
_ParamData_decltpe = Core.Name "decltpe"
_ParamData_default = Core.Name "default"
data Type =
  TypeRef RefType |
  TypeAnonymousName AnonymousNameType |
  TypeApply ApplyType |
  TypeApplyInfix ApplyInfixType |
  TypeFunction FunctionType |
  TypeContextFunction ContextFunctionType |
  TypePolyFunction PolyFunctionType |
  TypeImplicitFunction ImplicitFunctionType |
  TypeTuple TupleType |
  TypeWith WithType |
  TypeAnd AndType |
  TypeOr OrType |
  TypeRefine RefineType |
  TypeExistential ExistentialType |
  TypeAnnotate AnnotateType |
  TypeLambda LambdaType |
  TypeMethod MethodType |
  TypePlaceholder PlaceholderType |
  TypeByName ByNameType |
  TypeRepeated RepeatedType |
  TypeVar VarType |
  TypeTypedParam TypedParamType |
  TypeMatch MatchType
  deriving (Eq, Ord, Read, Show)
_Type = Core.Name "hydra.scala.syntax.Type"
_Type_ref = Core.Name "ref"
_Type_anonymousName = Core.Name "anonymousName"
_Type_apply = Core.Name "apply"
_Type_applyInfix = Core.Name "applyInfix"
_Type_function = Core.Name "function"
_Type_contextFunction = Core.Name "contextFunction"
_Type_polyFunction = Core.Name "polyFunction"
_Type_implicitFunction = Core.Name "implicitFunction"
_Type_tuple = Core.Name "tuple"
_Type_with = Core.Name "with"
_Type_and = Core.Name "and"
_Type_or = Core.Name "or"
_Type_refine = Core.Name "refine"
_Type_existential = Core.Name "existential"
_Type_annotate = Core.Name "annotate"
_Type_lambda = Core.Name "lambda"
_Type_method = Core.Name "method"
_Type_placeholder = Core.Name "placeholder"
_Type_byName = Core.Name "byName"
_Type_repeated = Core.Name "repeated"
_Type_var = Core.Name "var"
_Type_typedParam = Core.Name "typedParam"
_Type_match = Core.Name "match"
data RefType =
  RefTypeName NameType |
  RefTypeSelect SelectType |
  RefTypeProject ProjectType |
  RefTypeSingleton SingletonType
  deriving (Eq, Ord, Read, Show)
_RefType = Core.Name "hydra.scala.syntax.RefType"
_RefType_name = Core.Name "name"
_RefType_select = Core.Name "select"
_RefType_project = Core.Name "project"
_RefType_singleton = Core.Name "singleton"
data NameType =
  NameType {
    nameTypeValue :: String}
  deriving (Eq, Ord, Read, Show)
_NameType = Core.Name "hydra.scala.syntax.NameType"
_NameType_value = Core.Name "value"
newtype AnonymousNameType =
  AnonymousNameType {
    unAnonymousNameType :: ()}
  deriving (Eq, Ord, Read, Show)
_AnonymousNameType = Core.Name "hydra.scala.syntax.AnonymousNameType"
data SelectType =
  SelectType {
    selectTypeQual :: RefData,
    selectTypeName :: NameType}
  deriving (Eq, Ord, Read, Show)
_SelectType = Core.Name "hydra.scala.syntax.SelectType"
_SelectType_qual = Core.Name "qual"
_SelectType_name = Core.Name "name"
data ProjectType =
  ProjectType {
    projectTypeQual :: Type,
    projectTypeName :: NameType}
  deriving (Eq, Ord, Read, Show)
_ProjectType = Core.Name "hydra.scala.syntax.ProjectType"
_ProjectType_qual = Core.Name "qual"
_ProjectType_name = Core.Name "name"
data SingletonType =
  SingletonType {
    singletonTypeRef :: RefData}
  deriving (Eq, Ord, Read, Show)
_SingletonType = Core.Name "hydra.scala.syntax.SingletonType"
_SingletonType_ref = Core.Name "ref"
data ApplyType =
  ApplyType {
    applyTypeTpe :: Type,
    applyTypeArgs :: [Type]}
  deriving (Eq, Ord, Read, Show)
_ApplyType = Core.Name "hydra.scala.syntax.ApplyType"
_ApplyType_tpe = Core.Name "tpe"
_ApplyType_args = Core.Name "args"
data ApplyInfixType =
  ApplyInfixType {
    applyInfixTypeLhs :: Type,
    applyInfixTypeOp :: NameType,
    applyInfixTypeRhs :: Type}
  deriving (Eq, Ord, Read, Show)
_ApplyInfixType = Core.Name "hydra.scala.syntax.ApplyInfixType"
_ApplyInfixType_lhs = Core.Name "lhs"
_ApplyInfixType_op = Core.Name "op"
_ApplyInfixType_rhs = Core.Name "rhs"
data FunctionType =
  FunctionType {
    functionTypeParams :: [Type],
    functionTypeRes :: Type}
  deriving (Eq, Ord, Read, Show)
_FunctionType = Core.Name "hydra.scala.syntax.FunctionType"
_FunctionType_params = Core.Name "params"
_FunctionType_res = Core.Name "res"
data PolyFunctionType =
  PolyFunctionType {
    polyFunctionTypeTparams :: [ParamType],
    polyFunctionTypeTpe :: Type}
  deriving (Eq, Ord, Read, Show)
_PolyFunctionType = Core.Name "hydra.scala.syntax.PolyFunctionType"
_PolyFunctionType_tparams = Core.Name "tparams"
_PolyFunctionType_tpe = Core.Name "tpe"
data ContextFunctionType =
  ContextFunctionType {
    contextFunctionTypeParams :: [Type],
    contextFunctionTypeRes :: Type}
  deriving (Eq, Ord, Read, Show)
_ContextFunctionType = Core.Name "hydra.scala.syntax.ContextFunctionType"
_ContextFunctionType_params = Core.Name "params"
_ContextFunctionType_res = Core.Name "res"
data ImplicitFunctionType =
  ImplicitFunctionType {
    implicitFunctionTypeParams :: [Type],
    implicitFunctionTypeRes :: Type}
  deriving (Eq, Ord, Read, Show)
_ImplicitFunctionType = Core.Name "hydra.scala.syntax.ImplicitFunctionType"
_ImplicitFunctionType_params = Core.Name "params"
_ImplicitFunctionType_res = Core.Name "res"
data TupleType =
  TupleType {
    tupleTypeArgs :: [Type]}
  deriving (Eq, Ord, Read, Show)
_TupleType = Core.Name "hydra.scala.syntax.TupleType"
_TupleType_args = Core.Name "args"
data WithType =
  WithType {
    withTypeLhs :: Type,
    withTypeRhs :: Type}
  deriving (Eq, Ord, Read, Show)
_WithType = Core.Name "hydra.scala.syntax.WithType"
_WithType_lhs = Core.Name "lhs"
_WithType_rhs = Core.Name "rhs"
data AndType =
  AndType {
    andTypeLhs :: Type,
    andTypeRhs :: Type}
  deriving (Eq, Ord, Read, Show)
_AndType = Core.Name "hydra.scala.syntax.AndType"
_AndType_lhs = Core.Name "lhs"
_AndType_rhs = Core.Name "rhs"
data OrType =
  OrType {
    orTypeLhs :: Type,
    orTypeRhs :: Type}
  deriving (Eq, Ord, Read, Show)
_OrType = Core.Name "hydra.scala.syntax.OrType"
_OrType_lhs = Core.Name "lhs"
_OrType_rhs = Core.Name "rhs"
data RefineType =
  RefineType {
    refineTypeTpe :: (Maybe Type),
    refineTypeStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)
_RefineType = Core.Name "hydra.scala.syntax.RefineType"
_RefineType_tpe = Core.Name "tpe"
_RefineType_stats = Core.Name "stats"
data ExistentialType =
  ExistentialType {
    existentialTypeTpe :: Type,
    existentialTypeStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)
_ExistentialType = Core.Name "hydra.scala.syntax.ExistentialType"
_ExistentialType_tpe = Core.Name "tpe"
_ExistentialType_stats = Core.Name "stats"
data AnnotateType =
  AnnotateType {
    annotateTypeTpe :: Type,
    annotateTypeAnnots :: [AnnotMod]}
  deriving (Eq, Ord, Read, Show)
_AnnotateType = Core.Name "hydra.scala.syntax.AnnotateType"
_AnnotateType_tpe = Core.Name "tpe"
_AnnotateType_annots = Core.Name "annots"
data LambdaType =
  LambdaType {
    lambdaTypeTparams :: [ParamType],
    lambdaTypeTpe :: Type}
  deriving (Eq, Ord, Read, Show)
_LambdaType = Core.Name "hydra.scala.syntax.LambdaType"
_LambdaType_tparams = Core.Name "tparams"
_LambdaType_tpe = Core.Name "tpe"
data MethodType =
  MethodType {
    methodTypeParamss :: [[ParamData]],
    methodTypeTpe :: Type}
  deriving (Eq, Ord, Read, Show)
_MethodType = Core.Name "hydra.scala.syntax.MethodType"
_MethodType_paramss = Core.Name "paramss"
_MethodType_tpe = Core.Name "tpe"
data PlaceholderType =
  PlaceholderType {
    placeholderTypeBounds :: TypeBounds}
  deriving (Eq, Ord, Read, Show)
_PlaceholderType = Core.Name "hydra.scala.syntax.PlaceholderType"
_PlaceholderType_bounds = Core.Name "bounds"
data TypeBounds =
  TypeBounds {
    typeBoundsLo :: (Maybe Type),
    typeBoundsHi :: (Maybe Type)}
  deriving (Eq, Ord, Read, Show)
_TypeBounds = Core.Name "hydra.scala.syntax.TypeBounds"
_TypeBounds_lo = Core.Name "lo"
_TypeBounds_hi = Core.Name "hi"
data ByNameType =
  ByNameType {
    byNameTypeTpe :: Type}
  deriving (Eq, Ord, Read, Show)
_ByNameType = Core.Name "hydra.scala.syntax.ByNameType"
_ByNameType_tpe = Core.Name "tpe"
data RepeatedType =
  RepeatedType {
    repeatedTypeTpe :: Type}
  deriving (Eq, Ord, Read, Show)
_RepeatedType = Core.Name "hydra.scala.syntax.RepeatedType"
_RepeatedType_tpe = Core.Name "tpe"
data VarType =
  VarType {
    varTypeName :: NameType}
  deriving (Eq, Ord, Read, Show)
_VarType = Core.Name "hydra.scala.syntax.VarType"
_VarType_name = Core.Name "name"
data TypedParamType =
  TypedParamType {
    typedParamTypeName :: Name,
    typedParamTypeTyp :: Type}
  deriving (Eq, Ord, Read, Show)
_TypedParamType = Core.Name "hydra.scala.syntax.TypedParamType"
_TypedParamType_name = Core.Name "name"
_TypedParamType_typ = Core.Name "typ"
data ParamType =
  ParamType {
    paramTypeMods :: [Mod],
    paramTypeName :: Name,
    paramTypeTparams :: [ParamType],
    paramTypeTbounds :: [TypeBounds],
    paramTypeVbounds :: [Type],
    paramTypeCbounds :: [Type]}
  deriving (Eq, Ord, Read, Show)
_ParamType = Core.Name "hydra.scala.syntax.ParamType"
_ParamType_mods = Core.Name "mods"
_ParamType_name = Core.Name "name"
_ParamType_tparams = Core.Name "tparams"
_ParamType_tbounds = Core.Name "tbounds"
_ParamType_vbounds = Core.Name "vbounds"
_ParamType_cbounds = Core.Name "cbounds"
data MatchType =
  MatchType {
    matchTypeTpe :: Type,
    matchTypeCases :: [TypeCase]}
  deriving (Eq, Ord, Read, Show)
_MatchType = Core.Name "hydra.scala.syntax.MatchType"
_MatchType_tpe = Core.Name "tpe"
_MatchType_cases = Core.Name "cases"
data Pat =
  PatVar VarPat |
  PatWildcard |
  PatSeqWildcard |
  PatBind BindPat |
  PatAlternative AlternativePat |
  PatTuple TuplePat |
  PatRepeated RepeatedPat |
  PatExtract ExtractPat |
  PatExtractInfix ExtractInfixPat |
  PatInterpolate InterpolatePat |
  PatTyped TypedPat |
  PatGiven GivenPat
  deriving (Eq, Ord, Read, Show)
_Pat = Core.Name "hydra.scala.syntax.Pat"
_Pat_var = Core.Name "var"
_Pat_wildcard = Core.Name "wildcard"
_Pat_seqWildcard = Core.Name "seqWildcard"
_Pat_bind = Core.Name "bind"
_Pat_alternative = Core.Name "alternative"
_Pat_tuple = Core.Name "tuple"
_Pat_repeated = Core.Name "repeated"
_Pat_extract = Core.Name "extract"
_Pat_extractInfix = Core.Name "extractInfix"
_Pat_interpolate = Core.Name "interpolate"
_Pat_typed = Core.Name "typed"
_Pat_given = Core.Name "given"
data VarPat =
  VarPat {
    varPatName :: NameData}
  deriving (Eq, Ord, Read, Show)
_VarPat = Core.Name "hydra.scala.syntax.VarPat"
_VarPat_name = Core.Name "name"
data BindPat =
  BindPat {
    bindPatLhs :: Pat,
    bindPatRhs :: Pat}
  deriving (Eq, Ord, Read, Show)
_BindPat = Core.Name "hydra.scala.syntax.BindPat"
_BindPat_lhs = Core.Name "lhs"
_BindPat_rhs = Core.Name "rhs"
data AlternativePat =
  AlternativePat {
    alternativePatLhs :: Pat,
    alternativePatRhs :: Pat}
  deriving (Eq, Ord, Read, Show)
_AlternativePat = Core.Name "hydra.scala.syntax.AlternativePat"
_AlternativePat_lhs = Core.Name "lhs"
_AlternativePat_rhs = Core.Name "rhs"
data TuplePat =
  TuplePat {
    tuplePatArgs :: [Pat]}
  deriving (Eq, Ord, Read, Show)
_TuplePat = Core.Name "hydra.scala.syntax.TuplePat"
_TuplePat_args = Core.Name "args"
data RepeatedPat =
  RepeatedPat {
    repeatedPatName :: NameData}
  deriving (Eq, Ord, Read, Show)
_RepeatedPat = Core.Name "hydra.scala.syntax.RepeatedPat"
_RepeatedPat_name = Core.Name "name"
data ExtractPat =
  ExtractPat {
    extractPatFun :: Data,
    extractPatArgs :: [Pat]}
  deriving (Eq, Ord, Read, Show)
_ExtractPat = Core.Name "hydra.scala.syntax.ExtractPat"
_ExtractPat_fun = Core.Name "fun"
_ExtractPat_args = Core.Name "args"
data ExtractInfixPat =
  ExtractInfixPat {
    extractInfixPatLhs :: Pat,
    extractInfixPatOp :: NameData,
    extractInfixPatRhs :: [Pat]}
  deriving (Eq, Ord, Read, Show)
_ExtractInfixPat = Core.Name "hydra.scala.syntax.ExtractInfixPat"
_ExtractInfixPat_lhs = Core.Name "lhs"
_ExtractInfixPat_op = Core.Name "op"
_ExtractInfixPat_rhs = Core.Name "rhs"
data InterpolatePat =
  InterpolatePat {
    interpolatePatPrefix :: NameData,
    interpolatePatParts :: [Lit]}
  deriving (Eq, Ord, Read, Show)
_InterpolatePat = Core.Name "hydra.scala.syntax.InterpolatePat"
_InterpolatePat_prefix = Core.Name "prefix"
_InterpolatePat_parts = Core.Name "parts"
data TypedPat =
  TypedPat {
    typedPatLhs :: Pat,
    typedPatRhs :: Type}
  deriving (Eq, Ord, Read, Show)
_TypedPat = Core.Name "hydra.scala.syntax.TypedPat"
_TypedPat_lhs = Core.Name "lhs"
_TypedPat_rhs = Core.Name "rhs"
data GivenPat =
  GivenPat {
    givenPatTpe :: Type}
  deriving (Eq, Ord, Read, Show)
_GivenPat = Core.Name "hydra.scala.syntax.GivenPat"
_GivenPat_tpe = Core.Name "tpe"
data Member =
  MemberTerm DataMember |
  MemberType TypeMember |
  MemberTermParam ParamData |
  MemberTypeParam ParamType |
  MemberSelf Self
  deriving (Eq, Ord, Read, Show)
_Member = Core.Name "hydra.scala.syntax.Member"
_Member_term = Core.Name "term"
_Member_type = Core.Name "type"
_Member_termParam = Core.Name "termParam"
_Member_typeParam = Core.Name "typeParam"
_Member_self = Core.Name "self"
data DataMember =
  DataMemberPkg Pkg |
  DataMemberObject ObjectPkg
  deriving (Eq, Ord, Read, Show)
_DataMember = Core.Name "hydra.scala.syntax.DataMember"
_DataMember_pkg = Core.Name "pkg"
_DataMember_object = Core.Name "object"
data TypeMember =
  TypeMember {
    typeMemberName :: NameType}
  deriving (Eq, Ord, Read, Show)
_TypeMember = Core.Name "hydra.scala.syntax.TypeMember"
_TypeMember_name = Core.Name "name"
data Decl =
  DeclVal ValDecl |
  DeclVar VarDecl |
  DeclDef DefDecl |
  DeclType TypeDecl |
  DeclGiven GivenDecl
  deriving (Eq, Ord, Read, Show)
_Decl = Core.Name "hydra.scala.syntax.Decl"
_Decl_val = Core.Name "val"
_Decl_var = Core.Name "var"
_Decl_def = Core.Name "def"
_Decl_type = Core.Name "type"
_Decl_given = Core.Name "given"
data ValDecl =
  ValDecl {
    valDeclMods :: [Mod],
    valDeclPats :: [Pat],
    valDeclDecltpe :: Type}
  deriving (Eq, Ord, Read, Show)
_ValDecl = Core.Name "hydra.scala.syntax.ValDecl"
_ValDecl_mods = Core.Name "mods"
_ValDecl_pats = Core.Name "pats"
_ValDecl_decltpe = Core.Name "decltpe"
data VarDecl =
  VarDecl {
    varDeclMods :: [Mod],
    varDeclPats :: [Pat],
    varDeclDecltpe :: Type}
  deriving (Eq, Ord, Read, Show)
_VarDecl = Core.Name "hydra.scala.syntax.VarDecl"
_VarDecl_mods = Core.Name "mods"
_VarDecl_pats = Core.Name "pats"
_VarDecl_decltpe = Core.Name "decltpe"
data DefDecl =
  DefDecl {
    defDeclMods :: [Mod],
    defDeclName :: NameData,
    defDeclTparams :: [ParamType],
    defDeclParamss :: [[ParamData]],
    defDeclDecltpe :: Type}
  deriving (Eq, Ord, Read, Show)
_DefDecl = Core.Name "hydra.scala.syntax.DefDecl"
_DefDecl_mods = Core.Name "mods"
_DefDecl_name = Core.Name "name"
_DefDecl_tparams = Core.Name "tparams"
_DefDecl_paramss = Core.Name "paramss"
_DefDecl_decltpe = Core.Name "decltpe"
data TypeDecl =
  TypeDecl {
    typeDeclMods :: [Mod],
    typeDeclName :: NameType,
    typeDeclTparams :: [ParamType],
    typeDeclBounds :: TypeBounds}
  deriving (Eq, Ord, Read, Show)
_TypeDecl = Core.Name "hydra.scala.syntax.TypeDecl"
_TypeDecl_mods = Core.Name "mods"
_TypeDecl_name = Core.Name "name"
_TypeDecl_tparams = Core.Name "tparams"
_TypeDecl_bounds = Core.Name "bounds"
data GivenDecl =
  GivenDecl {
    givenDeclMods :: [Mod],
    givenDeclName :: NameData,
    givenDeclTparams :: [ParamType],
    givenDeclSparams :: [[ParamData]],
    givenDeclDecltpe :: Type}
  deriving (Eq, Ord, Read, Show)
_GivenDecl = Core.Name "hydra.scala.syntax.GivenDecl"
_GivenDecl_mods = Core.Name "mods"
_GivenDecl_name = Core.Name "name"
_GivenDecl_tparams = Core.Name "tparams"
_GivenDecl_sparams = Core.Name "sparams"
_GivenDecl_decltpe = Core.Name "decltpe"
data Defn =
  DefnVal ValDefn |
  DefnVar VarDefn |
  DefnGiven GivenDefn |
  DefnEnum EnumDefn |
  DefnEnumCase EnumCaseDefn |
  DefnRepeatedEnumCase RepeatedEnumCaseDefn |
  DefnGivenAlias GivenAliasDefn |
  DefnExtensionGroup ExtensionGroupDefn |
  DefnDef DefDefn |
  DefnType TypeDefn |
  DefnClass ClassDefn |
  DefnTrait TraitDefn |
  DefnObject ObjectDefn
  deriving (Eq, Ord, Read, Show)
_Defn = Core.Name "hydra.scala.syntax.Defn"
_Defn_val = Core.Name "val"
_Defn_var = Core.Name "var"
_Defn_given = Core.Name "given"
_Defn_enum = Core.Name "enum"
_Defn_enumCase = Core.Name "enumCase"
_Defn_repeatedEnumCase = Core.Name "repeatedEnumCase"
_Defn_givenAlias = Core.Name "givenAlias"
_Defn_extensionGroup = Core.Name "extensionGroup"
_Defn_def = Core.Name "def"
_Defn_type = Core.Name "type"
_Defn_class = Core.Name "class"
_Defn_trait = Core.Name "trait"
_Defn_object = Core.Name "object"
data ValDefn =
  ValDefn {
    valDefnMods :: [Mod],
    valDefnPats :: [Pat],
    valDefnDecltpe :: (Maybe Type),
    valDefnRhs :: Data}
  deriving (Eq, Ord, Read, Show)
_ValDefn = Core.Name "hydra.scala.syntax.ValDefn"
_ValDefn_mods = Core.Name "mods"
_ValDefn_pats = Core.Name "pats"
_ValDefn_decltpe = Core.Name "decltpe"
_ValDefn_rhs = Core.Name "rhs"
data VarDefn =
  VarDefn {
    varDefnMods :: [Mod],
    varDefnPats :: [Pat],
    varDefnDecltpe :: Type,
    varDefnRhs :: (Maybe Data)}
  deriving (Eq, Ord, Read, Show)
_VarDefn = Core.Name "hydra.scala.syntax.VarDefn"
_VarDefn_mods = Core.Name "mods"
_VarDefn_pats = Core.Name "pats"
_VarDefn_decltpe = Core.Name "decltpe"
_VarDefn_rhs = Core.Name "rhs"
data GivenDefn =
  GivenDefn {
    givenDefnMods :: [Mod],
    givenDefnName :: Name,
    givenDefnTparams :: [[ParamType]],
    givenDefnSparams :: [[ParamData]],
    givenDefnTempl :: Template}
  deriving (Eq, Ord, Read, Show)
_GivenDefn = Core.Name "hydra.scala.syntax.GivenDefn"
_GivenDefn_mods = Core.Name "mods"
_GivenDefn_name = Core.Name "name"
_GivenDefn_tparams = Core.Name "tparams"
_GivenDefn_sparams = Core.Name "sparams"
_GivenDefn_templ = Core.Name "templ"
data EnumDefn =
  EnumDefn {
    enumDefnMods :: [Mod],
    enumDefnName :: NameType,
    enumDefnTparams :: [ParamType],
    enumDefnCtor :: PrimaryCtor,
    enumDefnTemplate :: Template}
  deriving (Eq, Ord, Read, Show)
_EnumDefn = Core.Name "hydra.scala.syntax.EnumDefn"
_EnumDefn_mods = Core.Name "mods"
_EnumDefn_name = Core.Name "name"
_EnumDefn_tparams = Core.Name "tparams"
_EnumDefn_ctor = Core.Name "ctor"
_EnumDefn_template = Core.Name "template"
data EnumCaseDefn =
  EnumCaseDefn {
    enumCaseDefnMods :: [Mod],
    enumCaseDefnName :: NameData,
    enumCaseDefnTparams :: [ParamType],
    enumCaseDefnCtor :: PrimaryCtor,
    enumCaseDefnInits :: [Init]}
  deriving (Eq, Ord, Read, Show)
_EnumCaseDefn = Core.Name "hydra.scala.syntax.EnumCaseDefn"
_EnumCaseDefn_mods = Core.Name "mods"
_EnumCaseDefn_name = Core.Name "name"
_EnumCaseDefn_tparams = Core.Name "tparams"
_EnumCaseDefn_ctor = Core.Name "ctor"
_EnumCaseDefn_inits = Core.Name "inits"
data RepeatedEnumCaseDefn =
  RepeatedEnumCaseDefn {
    repeatedEnumCaseDefnMods :: [Mod],
    repeatedEnumCaseDefnCases :: [NameData]}
  deriving (Eq, Ord, Read, Show)
_RepeatedEnumCaseDefn = Core.Name "hydra.scala.syntax.RepeatedEnumCaseDefn"
_RepeatedEnumCaseDefn_mods = Core.Name "mods"
_RepeatedEnumCaseDefn_cases = Core.Name "cases"
data GivenAliasDefn =
  GivenAliasDefn {
    givenAliasDefnMods :: [Mod],
    givenAliasDefnName :: Name,
    givenAliasDefnTparams :: [[ParamType]],
    givenAliasDefnSparams :: [[ParamData]],
    givenAliasDefnDecltpe :: Type,
    givenAliasDefnBody :: Data}
  deriving (Eq, Ord, Read, Show)
_GivenAliasDefn = Core.Name "hydra.scala.syntax.GivenAliasDefn"
_GivenAliasDefn_mods = Core.Name "mods"
_GivenAliasDefn_name = Core.Name "name"
_GivenAliasDefn_tparams = Core.Name "tparams"
_GivenAliasDefn_sparams = Core.Name "sparams"
_GivenAliasDefn_decltpe = Core.Name "decltpe"
_GivenAliasDefn_body = Core.Name "body"
data ExtensionGroupDefn =
  ExtensionGroupDefn {
    extensionGroupDefnTparams :: [ParamType],
    extensionGroupDefnParmss :: [[ParamData]],
    extensionGroupDefnBody :: Stat}
  deriving (Eq, Ord, Read, Show)
_ExtensionGroupDefn = Core.Name "hydra.scala.syntax.ExtensionGroupDefn"
_ExtensionGroupDefn_tparams = Core.Name "tparams"
_ExtensionGroupDefn_parmss = Core.Name "parmss"
_ExtensionGroupDefn_body = Core.Name "body"
data DefDefn =
  DefDefn {
    defDefnMods :: [Mod],
    defDefnName :: NameData,
    defDefnTparams :: [ParamType],
    defDefnParamss :: [[ParamData]],
    defDefnDecltpe :: (Maybe Type),
    defDefnBody :: Data}
  deriving (Eq, Ord, Read, Show)
_DefDefn = Core.Name "hydra.scala.syntax.DefDefn"
_DefDefn_mods = Core.Name "mods"
_DefDefn_name = Core.Name "name"
_DefDefn_tparams = Core.Name "tparams"
_DefDefn_paramss = Core.Name "paramss"
_DefDefn_decltpe = Core.Name "decltpe"
_DefDefn_body = Core.Name "body"
data TypeDefn =
  TypeDefn {
    typeDefnMods :: [Mod],
    typeDefnName :: NameType,
    typeDefnTparams :: [ParamType],
    typeDefnBody :: Type}
  deriving (Eq, Ord, Read, Show)
_TypeDefn = Core.Name "hydra.scala.syntax.TypeDefn"
_TypeDefn_mods = Core.Name "mods"
_TypeDefn_name = Core.Name "name"
_TypeDefn_tparams = Core.Name "tparams"
_TypeDefn_body = Core.Name "body"
data ClassDefn =
  ClassDefn {
    classDefnMods :: [Mod],
    classDefnName :: NameType,
    classDefnTparams :: [ParamType],
    classDefnCtor :: PrimaryCtor,
    classDefnTemplate :: Template}
  deriving (Eq, Ord, Read, Show)
_ClassDefn = Core.Name "hydra.scala.syntax.ClassDefn"
_ClassDefn_mods = Core.Name "mods"
_ClassDefn_name = Core.Name "name"
_ClassDefn_tparams = Core.Name "tparams"
_ClassDefn_ctor = Core.Name "ctor"
_ClassDefn_template = Core.Name "template"
data TraitDefn =
  TraitDefn {
    traitDefnMods :: [Mod],
    traitDefnName :: NameType,
    traitDefnTparams :: [ParamType],
    traitDefnCtor :: PrimaryCtor,
    traitDefnTemplate :: Template}
  deriving (Eq, Ord, Read, Show)
_TraitDefn = Core.Name "hydra.scala.syntax.TraitDefn"
_TraitDefn_mods = Core.Name "mods"
_TraitDefn_name = Core.Name "name"
_TraitDefn_tparams = Core.Name "tparams"
_TraitDefn_ctor = Core.Name "ctor"
_TraitDefn_template = Core.Name "template"
data ObjectDefn =
  ObjectDefn {
    objectDefnName :: NameData}
  deriving (Eq, Ord, Read, Show)
_ObjectDefn = Core.Name "hydra.scala.syntax.ObjectDefn"
_ObjectDefn_name = Core.Name "name"
data Pkg =
  Pkg {
    pkgName :: NameData,
    pkgRef :: RefData,
    pkgStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)
_Pkg = Core.Name "hydra.scala.syntax.Pkg"
_Pkg_name = Core.Name "name"
_Pkg_ref = Core.Name "ref"
_Pkg_stats = Core.Name "stats"
data ObjectPkg =
  ObjectPkg {
    objectPkgMods :: [Mod],
    objectPkgName :: NameData,
    objectPkgTemplate :: Template}
  deriving (Eq, Ord, Read, Show)
_ObjectPkg = Core.Name "hydra.scala.syntax.ObjectPkg"
_ObjectPkg_mods = Core.Name "mods"
_ObjectPkg_name = Core.Name "name"
_ObjectPkg_template = Core.Name "template"
data Ctor =
  CtorPrimary PrimaryCtor |
  CtorSecondary SecondaryCtor
  deriving (Eq, Ord, Read, Show)
_Ctor = Core.Name "hydra.scala.syntax.Ctor"
_Ctor_primary = Core.Name "primary"
_Ctor_secondary = Core.Name "secondary"
data PrimaryCtor =
  PrimaryCtor {
    primaryCtorMods :: [Mod],
    primaryCtorName :: Name,
    primaryCtorParamss :: [[ParamData]]}
  deriving (Eq, Ord, Read, Show)
_PrimaryCtor = Core.Name "hydra.scala.syntax.PrimaryCtor"
_PrimaryCtor_mods = Core.Name "mods"
_PrimaryCtor_name = Core.Name "name"
_PrimaryCtor_paramss = Core.Name "paramss"
data SecondaryCtor =
  SecondaryCtor {
    secondaryCtorMods :: [Mod],
    secondaryCtorName :: Name,
    secondaryCtorParamss :: [[ParamData]],
    secondaryCtorInit :: Init,
    secondaryCtorStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)
_SecondaryCtor = Core.Name "hydra.scala.syntax.SecondaryCtor"
_SecondaryCtor_mods = Core.Name "mods"
_SecondaryCtor_name = Core.Name "name"
_SecondaryCtor_paramss = Core.Name "paramss"
_SecondaryCtor_init = Core.Name "init"
_SecondaryCtor_stats = Core.Name "stats"
data Init =
  Init {
    initTpe :: Type,
    initName :: Name,
    initArgss :: [[Data]]}
  deriving (Eq, Ord, Read, Show)
_Init = Core.Name "hydra.scala.syntax.Init"
_Init_tpe = Core.Name "tpe"
_Init_name = Core.Name "name"
_Init_argss = Core.Name "argss"
newtype Self =
  Self {
    unSelf :: ()}
  deriving (Eq, Ord, Read, Show)
_Self = Core.Name "hydra.scala.syntax.Self"
data Template =
  Template {
    templateEarly :: [Stat],
    templateInits :: [Init],
    templateSelf :: Self,
    templateStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)
_Template = Core.Name "hydra.scala.syntax.Template"
_Template_early = Core.Name "early"
_Template_inits = Core.Name "inits"
_Template_self = Core.Name "self"
_Template_stats = Core.Name "stats"
data Mod =
  ModAnnot AnnotMod |
  ModPrivate PrivateMod |
  ModProtected ProtectedMod |
  ModImplicit |
  ModFinal |
  ModSealed |
  ModOpen |
  ModSuper |
  ModOverride |
  ModCase |
  ModAbstract |
  ModCovariant |
  ModContravariant |
  ModLazy |
  ModValParam |
  ModVarParam |
  ModInfix |
  ModInline |
  ModUsing |
  ModOpaque |
  ModTransparent
  deriving (Eq, Ord, Read, Show)
_Mod = Core.Name "hydra.scala.syntax.Mod"
_Mod_annot = Core.Name "annot"
_Mod_private = Core.Name "private"
_Mod_protected = Core.Name "protected"
_Mod_implicit = Core.Name "implicit"
_Mod_final = Core.Name "final"
_Mod_sealed = Core.Name "sealed"
_Mod_open = Core.Name "open"
_Mod_super = Core.Name "super"
_Mod_override = Core.Name "override"
_Mod_case = Core.Name "case"
_Mod_abstract = Core.Name "abstract"
_Mod_covariant = Core.Name "covariant"
_Mod_contravariant = Core.Name "contravariant"
_Mod_lazy = Core.Name "lazy"
_Mod_valParam = Core.Name "valParam"
_Mod_varParam = Core.Name "varParam"
_Mod_infix = Core.Name "infix"
_Mod_inline = Core.Name "inline"
_Mod_using = Core.Name "using"
_Mod_opaque = Core.Name "opaque"
_Mod_transparent = Core.Name "transparent"
data AnnotMod =
  AnnotMod {
    annotModInit :: Init}
  deriving (Eq, Ord, Read, Show)
_AnnotMod = Core.Name "hydra.scala.syntax.AnnotMod"
_AnnotMod_init = Core.Name "init"
data PrivateMod =
  PrivateMod {
    privateModWithin :: Ref}
  deriving (Eq, Ord, Read, Show)
_PrivateMod = Core.Name "hydra.scala.syntax.PrivateMod"
_PrivateMod_within = Core.Name "within"
data ProtectedMod =
  ProtectedMod {
    protectedModWithin :: Ref}
  deriving (Eq, Ord, Read, Show)
_ProtectedMod = Core.Name "hydra.scala.syntax.ProtectedMod"
_ProtectedMod_within = Core.Name "within"
data Enumerator =
  EnumeratorGenerator GeneratorEnumerator |
  EnumeratorCaseGenerator CaseGeneratorEnumerator |
  EnumeratorVal ValEnumerator |
  EnumeratorGuard GuardEnumerator
  deriving (Eq, Ord, Read, Show)
_Enumerator = Core.Name "hydra.scala.syntax.Enumerator"
_Enumerator_generator = Core.Name "generator"
_Enumerator_caseGenerator = Core.Name "caseGenerator"
_Enumerator_val = Core.Name "val"
_Enumerator_guard = Core.Name "guard"
data GeneratorEnumerator =
  GeneratorEnumerator {
    generatorEnumeratorPat :: Pat,
    generatorEnumeratorRhs :: Data}
  deriving (Eq, Ord, Read, Show)
_GeneratorEnumerator = Core.Name "hydra.scala.syntax.GeneratorEnumerator"
_GeneratorEnumerator_pat = Core.Name "pat"
_GeneratorEnumerator_rhs = Core.Name "rhs"
data CaseGeneratorEnumerator =
  CaseGeneratorEnumerator {
    caseGeneratorEnumeratorPat :: Pat,
    caseGeneratorEnumeratorRhs :: Data}
  deriving (Eq, Ord, Read, Show)
_CaseGeneratorEnumerator = Core.Name "hydra.scala.syntax.CaseGeneratorEnumerator"
_CaseGeneratorEnumerator_pat = Core.Name "pat"
_CaseGeneratorEnumerator_rhs = Core.Name "rhs"
data ValEnumerator =
  ValEnumerator {
    valEnumeratorPat :: Pat,
    valEnumeratorRhs :: Data}
  deriving (Eq, Ord, Read, Show)
_ValEnumerator = Core.Name "hydra.scala.syntax.ValEnumerator"
_ValEnumerator_pat = Core.Name "pat"
_ValEnumerator_rhs = Core.Name "rhs"
data GuardEnumerator =
  GuardEnumerator {
    guardEnumeratorCond :: Data}
  deriving (Eq, Ord, Read, Show)
_GuardEnumerator = Core.Name "hydra.scala.syntax.GuardEnumerator"
_GuardEnumerator_cond = Core.Name "cond"
data ImportExportStat =
  ImportExportStatImport Import |
  ImportExportStatExport Export
  deriving (Eq, Ord, Read, Show)
_ImportExportStat = Core.Name "hydra.scala.syntax.ImportExportStat"
_ImportExportStat_import = Core.Name "import"
_ImportExportStat_export = Core.Name "export"
data Import =
  Import {
    importImporters :: [Importer]}
  deriving (Eq, Ord, Read, Show)
_Import = Core.Name "hydra.scala.syntax.Import"
_Import_importers = Core.Name "importers"
data Export =
  Export {
    exportImporters :: [Importer]}
  deriving (Eq, Ord, Read, Show)
_Export = Core.Name "hydra.scala.syntax.Export"
_Export_importers = Core.Name "importers"
data Importer =
  Importer {
    importerRef :: RefData,
    importerImportees :: [Importee]}
  deriving (Eq, Ord, Read, Show)
_Importer = Core.Name "hydra.scala.syntax.Importer"
_Importer_ref = Core.Name "ref"
_Importer_importees = Core.Name "importees"
data Importee =
  ImporteeWildcard |
  ImporteeGiven GivenImportee |
  ImporteeGivenAll |
  ImporteeName NameImportee |
  ImporteeRename RenameImportee |
  ImporteeUnimport UnimportImportee
  deriving (Eq, Ord, Read, Show)
_Importee = Core.Name "hydra.scala.syntax.Importee"
_Importee_wildcard = Core.Name "wildcard"
_Importee_given = Core.Name "given"
_Importee_givenAll = Core.Name "givenAll"
_Importee_name = Core.Name "name"
_Importee_rename = Core.Name "rename"
_Importee_unimport = Core.Name "unimport"
data GivenImportee =
  GivenImportee {
    givenImporteeTpe :: Type}
  deriving (Eq, Ord, Read, Show)
_GivenImportee = Core.Name "hydra.scala.syntax.GivenImportee"
_GivenImportee_tpe = Core.Name "tpe"
data NameImportee =
  NameImportee {
    nameImporteeName :: Name}
  deriving (Eq, Ord, Read, Show)
_NameImportee = Core.Name "hydra.scala.syntax.NameImportee"
_NameImportee_name = Core.Name "name"
data RenameImportee =
  RenameImportee {
    renameImporteeName :: Name,
    renameImporteeRename :: Name}
  deriving (Eq, Ord, Read, Show)
_RenameImportee = Core.Name "hydra.scala.syntax.RenameImportee"
_RenameImportee_name = Core.Name "name"
_RenameImportee_rename = Core.Name "rename"
data UnimportImportee =
  UnimportImportee {
    unimportImporteeName :: Name}
  deriving (Eq, Ord, Read, Show)
_UnimportImportee = Core.Name "hydra.scala.syntax.UnimportImportee"
_UnimportImportee_name = Core.Name "name"
data CaseTree =
  CaseTreeCase Case |
  CaseTreeTypeCase TypeCase
  deriving (Eq, Ord, Read, Show)
_CaseTree = Core.Name "hydra.scala.syntax.CaseTree"
_CaseTree_case = Core.Name "case"
_CaseTree_typeCase = Core.Name "typeCase"
data Case =
  Case {
    casePat :: Pat,
    caseCond :: (Maybe Data),
    caseBody :: Data}
  deriving (Eq, Ord, Read, Show)
_Case = Core.Name "hydra.scala.syntax.Case"
_Case_pat = Core.Name "pat"
_Case_cond = Core.Name "cond"
_Case_body = Core.Name "body"
data TypeCase =
  TypeCase {
    typeCasePat :: Type,
    typeCaseBody :: Type}
  deriving (Eq, Ord, Read, Show)
_TypeCase = Core.Name "hydra.scala.syntax.TypeCase"
_TypeCase_pat = Core.Name "pat"
_TypeCase_body = Core.Name "body"
data Source =
  Source {
    sourceStats :: [Stat]}
  deriving (Eq, Ord, Read, Show)
_Source = Core.Name "hydra.scala.syntax.Source"
_Source_stats = Core.Name "stats"
