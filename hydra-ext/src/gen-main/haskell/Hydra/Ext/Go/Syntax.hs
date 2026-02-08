-- Note: this is an automatically generated file. Do not edit.

-- | A Go syntax model, based on the Go Language Specification retrieved on 2025-02-05 from https://go.dev/ref/spec

module Hydra.Ext.Go.Syntax where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data AnnotatedDeclaration = 
  AnnotatedDeclaration {
    annotatedDeclarationComment :: String,
    annotatedDeclarationDeclaration :: TopLevelDecl}
  deriving (Eq, Ord, Read, Show)

_AnnotatedDeclaration = (Core.Name "hydra.ext.go.syntax.AnnotatedDeclaration")

_AnnotatedDeclaration_comment = (Core.Name "comment")

_AnnotatedDeclaration_declaration = (Core.Name "declaration")

data Module = 
  Module {
    modulePackage :: PackageClause,
    moduleImports :: [ImportDecl],
    moduleDeclarations :: [TopLevelDecl]}
  deriving (Eq, Ord, Read, Show)

_Module = (Core.Name "hydra.ext.go.syntax.Module")

_Module_package = (Core.Name "package")

_Module_imports = (Core.Name "imports")

_Module_declarations = (Core.Name "declarations")

newtype Identifier = 
  Identifier {
    unIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_Identifier = (Core.Name "hydra.ext.go.syntax.Identifier")

newtype IntLit = 
  IntLit {
    unIntLit :: Integer}
  deriving (Eq, Ord, Read, Show)

_IntLit = (Core.Name "hydra.ext.go.syntax.IntLit")

newtype FloatLit = 
  FloatLit {
    unFloatLit :: Double}
  deriving (Eq, Ord, Read, Show)

_FloatLit = (Core.Name "hydra.ext.go.syntax.FloatLit")

newtype ImaginaryLit = 
  ImaginaryLit {
    unImaginaryLit :: Double}
  deriving (Eq, Ord, Read, Show)

_ImaginaryLit = (Core.Name "hydra.ext.go.syntax.ImaginaryLit")

newtype RuneLit = 
  RuneLit {
    unRuneLit :: Int}
  deriving (Eq, Ord, Read, Show)

_RuneLit = (Core.Name "hydra.ext.go.syntax.RuneLit")

data StringLit = 
  StringLitRaw RawStringLit |
  StringLitInterpreted InterpretedStringLit
  deriving (Eq, Ord, Read, Show)

_StringLit = (Core.Name "hydra.ext.go.syntax.StringLit")

_StringLit_raw = (Core.Name "raw")

_StringLit_interpreted = (Core.Name "interpreted")

newtype RawStringLit = 
  RawStringLit {
    unRawStringLit :: String}
  deriving (Eq, Ord, Read, Show)

_RawStringLit = (Core.Name "hydra.ext.go.syntax.RawStringLit")

newtype InterpretedStringLit = 
  InterpretedStringLit {
    unInterpretedStringLit :: String}
  deriving (Eq, Ord, Read, Show)

_InterpretedStringLit = (Core.Name "hydra.ext.go.syntax.InterpretedStringLit")

data SourceFile = 
  SourceFile {
    sourceFilePackage :: PackageClause,
    sourceFileImports :: [ImportDecl],
    sourceFileDeclarations :: [TopLevelDecl]}
  deriving (Eq, Ord, Read, Show)

_SourceFile = (Core.Name "hydra.ext.go.syntax.SourceFile")

_SourceFile_package = (Core.Name "package")

_SourceFile_imports = (Core.Name "imports")

_SourceFile_declarations = (Core.Name "declarations")

newtype PackageClause = 
  PackageClause {
    unPackageClause :: Identifier}
  deriving (Eq, Ord, Read, Show)

_PackageClause = (Core.Name "hydra.ext.go.syntax.PackageClause")

newtype ImportDecl = 
  ImportDecl {
    unImportDecl :: [ImportSpec]}
  deriving (Eq, Ord, Read, Show)

_ImportDecl = (Core.Name "hydra.ext.go.syntax.ImportDecl")

data ImportSpec = 
  ImportSpec {
    importSpecAlias :: (Maybe ImportAlias),
    importSpecPath :: ImportPath}
  deriving (Eq, Ord, Read, Show)

_ImportSpec = (Core.Name "hydra.ext.go.syntax.ImportSpec")

_ImportSpec_alias = (Core.Name "alias")

_ImportSpec_path = (Core.Name "path")

data ImportAlias = 
  ImportAliasDot  |
  ImportAliasName Identifier
  deriving (Eq, Ord, Read, Show)

_ImportAlias = (Core.Name "hydra.ext.go.syntax.ImportAlias")

_ImportAlias_dot = (Core.Name "dot")

_ImportAlias_name = (Core.Name "name")

newtype ImportPath = 
  ImportPath {
    unImportPath :: StringLit}
  deriving (Eq, Ord, Read, Show)

_ImportPath = (Core.Name "hydra.ext.go.syntax.ImportPath")

data Declaration = 
  DeclarationConst ConstDecl |
  DeclarationType TypeDecl |
  DeclarationVar VarDecl
  deriving (Eq, Ord, Read, Show)

_Declaration = (Core.Name "hydra.ext.go.syntax.Declaration")

_Declaration_const = (Core.Name "const")

_Declaration_type = (Core.Name "type")

_Declaration_var = (Core.Name "var")

data TopLevelDecl = 
  TopLevelDeclDeclaration Declaration |
  TopLevelDeclFunction FunctionDecl |
  TopLevelDeclMethod MethodDecl
  deriving (Eq, Ord, Read, Show)

_TopLevelDecl = (Core.Name "hydra.ext.go.syntax.TopLevelDecl")

_TopLevelDecl_declaration = (Core.Name "declaration")

_TopLevelDecl_function = (Core.Name "function")

_TopLevelDecl_method = (Core.Name "method")

newtype ConstDecl = 
  ConstDecl {
    unConstDecl :: [ConstSpec]}
  deriving (Eq, Ord, Read, Show)

_ConstDecl = (Core.Name "hydra.ext.go.syntax.ConstDecl")

data ConstSpec = 
  ConstSpec {
    constSpecNames :: [Identifier],
    constSpecType :: (Maybe Type),
    constSpecValues :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_ConstSpec = (Core.Name "hydra.ext.go.syntax.ConstSpec")

_ConstSpec_names = (Core.Name "names")

_ConstSpec_type = (Core.Name "type")

_ConstSpec_values = (Core.Name "values")

newtype VarDecl = 
  VarDecl {
    unVarDecl :: [VarSpec]}
  deriving (Eq, Ord, Read, Show)

_VarDecl = (Core.Name "hydra.ext.go.syntax.VarDecl")

data VarSpec = 
  VarSpec {
    varSpecNames :: [Identifier],
    varSpecType :: (Maybe Type),
    varSpecValues :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_VarSpec = (Core.Name "hydra.ext.go.syntax.VarSpec")

_VarSpec_names = (Core.Name "names")

_VarSpec_type = (Core.Name "type")

_VarSpec_values = (Core.Name "values")

data ShortVarDecl = 
  ShortVarDecl {
    shortVarDeclNames :: [Identifier],
    shortVarDeclValues :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_ShortVarDecl = (Core.Name "hydra.ext.go.syntax.ShortVarDecl")

_ShortVarDecl_names = (Core.Name "names")

_ShortVarDecl_values = (Core.Name "values")

newtype TypeDecl = 
  TypeDecl {
    unTypeDecl :: [TypeSpec]}
  deriving (Eq, Ord, Read, Show)

_TypeDecl = (Core.Name "hydra.ext.go.syntax.TypeDecl")

data TypeSpec = 
  TypeSpecAlias AliasDecl |
  TypeSpecDefinition TypeDef
  deriving (Eq, Ord, Read, Show)

_TypeSpec = (Core.Name "hydra.ext.go.syntax.TypeSpec")

_TypeSpec_alias = (Core.Name "alias")

_TypeSpec_definition = (Core.Name "definition")

data AliasDecl = 
  AliasDecl {
    aliasDeclName :: Identifier,
    aliasDeclType :: Type}
  deriving (Eq, Ord, Read, Show)

_AliasDecl = (Core.Name "hydra.ext.go.syntax.AliasDecl")

_AliasDecl_name = (Core.Name "name")

_AliasDecl_type = (Core.Name "type")

data TypeDef = 
  TypeDef {
    typeDefName :: Identifier,
    typeDefTypeParams :: (Maybe TypeParameters),
    typeDefType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeDef = (Core.Name "hydra.ext.go.syntax.TypeDef")

_TypeDef_name = (Core.Name "name")

_TypeDef_typeParams = (Core.Name "typeParams")

_TypeDef_type = (Core.Name "type")

newtype TypeParameters = 
  TypeParameters {
    unTypeParameters :: [TypeParamDecl]}
  deriving (Eq, Ord, Read, Show)

_TypeParameters = (Core.Name "hydra.ext.go.syntax.TypeParameters")

data TypeParamDecl = 
  TypeParamDecl {
    typeParamDeclNames :: [Identifier],
    typeParamDeclConstraint :: TypeConstraint}
  deriving (Eq, Ord, Read, Show)

_TypeParamDecl = (Core.Name "hydra.ext.go.syntax.TypeParamDecl")

_TypeParamDecl_names = (Core.Name "names")

_TypeParamDecl_constraint = (Core.Name "constraint")

newtype TypeConstraint = 
  TypeConstraint {
    unTypeConstraint :: TypeElem}
  deriving (Eq, Ord, Read, Show)

_TypeConstraint = (Core.Name "hydra.ext.go.syntax.TypeConstraint")

data FunctionDecl = 
  FunctionDecl {
    functionDeclName :: Identifier,
    functionDeclTypeParams :: (Maybe TypeParameters),
    functionDeclSignature :: Signature,
    functionDeclBody :: (Maybe FunctionBody)}
  deriving (Eq, Ord, Read, Show)

_FunctionDecl = (Core.Name "hydra.ext.go.syntax.FunctionDecl")

_FunctionDecl_name = (Core.Name "name")

_FunctionDecl_typeParams = (Core.Name "typeParams")

_FunctionDecl_signature = (Core.Name "signature")

_FunctionDecl_body = (Core.Name "body")

newtype FunctionBody = 
  FunctionBody {
    unFunctionBody :: Block}
  deriving (Eq, Ord, Read, Show)

_FunctionBody = (Core.Name "hydra.ext.go.syntax.FunctionBody")

data MethodDecl = 
  MethodDecl {
    methodDeclReceiver :: Receiver,
    methodDeclName :: Identifier,
    methodDeclSignature :: Signature,
    methodDeclBody :: (Maybe FunctionBody)}
  deriving (Eq, Ord, Read, Show)

_MethodDecl = (Core.Name "hydra.ext.go.syntax.MethodDecl")

_MethodDecl_receiver = (Core.Name "receiver")

_MethodDecl_name = (Core.Name "name")

_MethodDecl_signature = (Core.Name "signature")

_MethodDecl_body = (Core.Name "body")

data Receiver = 
  Receiver {
    receiverName :: (Maybe Identifier),
    receiverType :: Type}
  deriving (Eq, Ord, Read, Show)

_Receiver = (Core.Name "hydra.ext.go.syntax.Receiver")

_Receiver_name = (Core.Name "name")

_Receiver_type = (Core.Name "type")

data Type = 
  TypeName_ TypeName |
  TypeLiteral TypeLit |
  TypeParen Type
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra.ext.go.syntax.Type")

_Type_name = (Core.Name "name")

_Type_literal = (Core.Name "literal")

_Type_paren = (Core.Name "paren")

data TypeName = 
  TypeName {
    typeNameName :: QualifiedIdent,
    typeNameTypeArgs :: [Type]}
  deriving (Eq, Ord, Read, Show)

_TypeName = (Core.Name "hydra.ext.go.syntax.TypeName")

_TypeName_name = (Core.Name "name")

_TypeName_typeArgs = (Core.Name "typeArgs")

data QualifiedIdent = 
  QualifiedIdent {
    qualifiedIdentPackage :: (Maybe Identifier),
    qualifiedIdentName :: Identifier}
  deriving (Eq, Ord, Read, Show)

_QualifiedIdent = (Core.Name "hydra.ext.go.syntax.QualifiedIdent")

_QualifiedIdent_package = (Core.Name "package")

_QualifiedIdent_name = (Core.Name "name")

data TypeLit = 
  TypeLitArray ArrayType |
  TypeLitStruct StructType |
  TypeLitPointer PointerType |
  TypeLitFunction FunctionType |
  TypeLitInterface InterfaceType |
  TypeLitSlice SliceType |
  TypeLitMap MapType |
  TypeLitChannel ChannelType
  deriving (Eq, Ord, Read, Show)

_TypeLit = (Core.Name "hydra.ext.go.syntax.TypeLit")

_TypeLit_array = (Core.Name "array")

_TypeLit_struct = (Core.Name "struct")

_TypeLit_pointer = (Core.Name "pointer")

_TypeLit_function = (Core.Name "function")

_TypeLit_interface = (Core.Name "interface")

_TypeLit_slice = (Core.Name "slice")

_TypeLit_map = (Core.Name "map")

_TypeLit_channel = (Core.Name "channel")

data ArrayType = 
  ArrayType {
    arrayTypeLength :: Expression,
    arrayTypeElement :: Type}
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra.ext.go.syntax.ArrayType")

_ArrayType_length = (Core.Name "length")

_ArrayType_element = (Core.Name "element")

newtype SliceType = 
  SliceType {
    unSliceType :: Type}
  deriving (Eq, Ord, Read, Show)

_SliceType = (Core.Name "hydra.ext.go.syntax.SliceType")

newtype StructType = 
  StructType {
    unStructType :: [FieldDecl]}
  deriving (Eq, Ord, Read, Show)

_StructType = (Core.Name "hydra.ext.go.syntax.StructType")

data FieldDecl = 
  FieldDeclNamed NamedField |
  FieldDeclEmbedded EmbeddedField
  deriving (Eq, Ord, Read, Show)

_FieldDecl = (Core.Name "hydra.ext.go.syntax.FieldDecl")

_FieldDecl_named = (Core.Name "named")

_FieldDecl_embedded = (Core.Name "embedded")

data NamedField = 
  NamedField {
    namedFieldNames :: [Identifier],
    namedFieldType :: Type,
    namedFieldTag :: (Maybe Tag)}
  deriving (Eq, Ord, Read, Show)

_NamedField = (Core.Name "hydra.ext.go.syntax.NamedField")

_NamedField_names = (Core.Name "names")

_NamedField_type = (Core.Name "type")

_NamedField_tag = (Core.Name "tag")

data EmbeddedField = 
  EmbeddedField {
    embeddedFieldPointer :: Bool,
    embeddedFieldType :: TypeName,
    embeddedFieldTag :: (Maybe Tag)}
  deriving (Eq, Ord, Read, Show)

_EmbeddedField = (Core.Name "hydra.ext.go.syntax.EmbeddedField")

_EmbeddedField_pointer = (Core.Name "pointer")

_EmbeddedField_type = (Core.Name "type")

_EmbeddedField_tag = (Core.Name "tag")

newtype Tag = 
  Tag {
    unTag :: StringLit}
  deriving (Eq, Ord, Read, Show)

_Tag = (Core.Name "hydra.ext.go.syntax.Tag")

newtype PointerType = 
  PointerType {
    unPointerType :: Type}
  deriving (Eq, Ord, Read, Show)

_PointerType = (Core.Name "hydra.ext.go.syntax.PointerType")

newtype FunctionType = 
  FunctionType {
    unFunctionType :: Signature}
  deriving (Eq, Ord, Read, Show)

_FunctionType = (Core.Name "hydra.ext.go.syntax.FunctionType")

data Signature = 
  Signature {
    signatureParameters :: Parameters,
    signatureResult :: (Maybe Result)}
  deriving (Eq, Ord, Read, Show)

_Signature = (Core.Name "hydra.ext.go.syntax.Signature")

_Signature_parameters = (Core.Name "parameters")

_Signature_result = (Core.Name "result")

data Result = 
  ResultParameters Parameters |
  ResultType Type
  deriving (Eq, Ord, Read, Show)

_Result = (Core.Name "hydra.ext.go.syntax.Result")

_Result_parameters = (Core.Name "parameters")

_Result_type = (Core.Name "type")

newtype Parameters = 
  Parameters {
    unParameters :: [ParameterDecl]}
  deriving (Eq, Ord, Read, Show)

_Parameters = (Core.Name "hydra.ext.go.syntax.Parameters")

data ParameterDecl = 
  ParameterDecl {
    parameterDeclNames :: [Identifier],
    parameterDeclVariadic :: Bool,
    parameterDeclType :: Type}
  deriving (Eq, Ord, Read, Show)

_ParameterDecl = (Core.Name "hydra.ext.go.syntax.ParameterDecl")

_ParameterDecl_names = (Core.Name "names")

_ParameterDecl_variadic = (Core.Name "variadic")

_ParameterDecl_type = (Core.Name "type")

newtype InterfaceType = 
  InterfaceType {
    unInterfaceType :: [InterfaceElem]}
  deriving (Eq, Ord, Read, Show)

_InterfaceType = (Core.Name "hydra.ext.go.syntax.InterfaceType")

data InterfaceElem = 
  InterfaceElemMethod MethodElem |
  InterfaceElemType TypeElem
  deriving (Eq, Ord, Read, Show)

_InterfaceElem = (Core.Name "hydra.ext.go.syntax.InterfaceElem")

_InterfaceElem_method = (Core.Name "method")

_InterfaceElem_type = (Core.Name "type")

data MethodElem = 
  MethodElem {
    methodElemName :: Identifier,
    methodElemSignature :: Signature}
  deriving (Eq, Ord, Read, Show)

_MethodElem = (Core.Name "hydra.ext.go.syntax.MethodElem")

_MethodElem_name = (Core.Name "name")

_MethodElem_signature = (Core.Name "signature")

newtype TypeElem = 
  TypeElem {
    unTypeElem :: [TypeTerm]}
  deriving (Eq, Ord, Read, Show)

_TypeElem = (Core.Name "hydra.ext.go.syntax.TypeElem")

data TypeTerm = 
  TypeTerm {
    typeTermUnderlying :: Bool,
    typeTermType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeTerm = (Core.Name "hydra.ext.go.syntax.TypeTerm")

_TypeTerm_underlying = (Core.Name "underlying")

_TypeTerm_type = (Core.Name "type")

data MapType = 
  MapType {
    mapTypeKey :: Type,
    mapTypeValue :: Type}
  deriving (Eq, Ord, Read, Show)

_MapType = (Core.Name "hydra.ext.go.syntax.MapType")

_MapType_key = (Core.Name "key")

_MapType_value = (Core.Name "value")

data ChannelType = 
  ChannelType {
    channelTypeDirection :: ChannelDirection,
    channelTypeElement :: Type}
  deriving (Eq, Ord, Read, Show)

_ChannelType = (Core.Name "hydra.ext.go.syntax.ChannelType")

_ChannelType_direction = (Core.Name "direction")

_ChannelType_element = (Core.Name "element")

data ChannelDirection = 
  ChannelDirectionBidirectional  |
  ChannelDirectionSend  |
  ChannelDirectionReceive 
  deriving (Eq, Ord, Read, Show)

_ChannelDirection = (Core.Name "hydra.ext.go.syntax.ChannelDirection")

_ChannelDirection_bidirectional = (Core.Name "bidirectional")

_ChannelDirection_send = (Core.Name "send")

_ChannelDirection_receive = (Core.Name "receive")

data Expression = 
  ExpressionUnary UnaryExpr |
  ExpressionBinary BinaryExpr
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra.ext.go.syntax.Expression")

_Expression_unary = (Core.Name "unary")

_Expression_binary = (Core.Name "binary")

data UnaryExpr = 
  UnaryExprPrimary PrimaryExpr |
  UnaryExprOp UnaryOperation
  deriving (Eq, Ord, Read, Show)

_UnaryExpr = (Core.Name "hydra.ext.go.syntax.UnaryExpr")

_UnaryExpr_primary = (Core.Name "primary")

_UnaryExpr_op = (Core.Name "op")

data UnaryOperation = 
  UnaryOperation {
    unaryOperationOp :: UnaryOp,
    unaryOperationOperand :: UnaryExpr}
  deriving (Eq, Ord, Read, Show)

_UnaryOperation = (Core.Name "hydra.ext.go.syntax.UnaryOperation")

_UnaryOperation_op = (Core.Name "op")

_UnaryOperation_operand = (Core.Name "operand")

data BinaryExpr = 
  BinaryExpr {
    binaryExprLeft :: Expression,
    binaryExprOp :: BinaryOp,
    binaryExprRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_BinaryExpr = (Core.Name "hydra.ext.go.syntax.BinaryExpr")

_BinaryExpr_left = (Core.Name "left")

_BinaryExpr_op = (Core.Name "op")

_BinaryExpr_right = (Core.Name "right")

data BinaryOp = 
  BinaryOpOr  |
  BinaryOpAnd  |
  BinaryOpEqual  |
  BinaryOpNotEqual  |
  BinaryOpLess  |
  BinaryOpLessEqual  |
  BinaryOpGreater  |
  BinaryOpGreaterEqual  |
  BinaryOpAdd  |
  BinaryOpSubtract  |
  BinaryOpBitwiseOr  |
  BinaryOpBitwiseXor  |
  BinaryOpMultiply  |
  BinaryOpDivide  |
  BinaryOpRemainder  |
  BinaryOpLeftShift  |
  BinaryOpRightShift  |
  BinaryOpBitwiseAnd  |
  BinaryOpBitClear 
  deriving (Eq, Ord, Read, Show)

_BinaryOp = (Core.Name "hydra.ext.go.syntax.BinaryOp")

_BinaryOp_or = (Core.Name "or")

_BinaryOp_and = (Core.Name "and")

_BinaryOp_equal = (Core.Name "equal")

_BinaryOp_notEqual = (Core.Name "notEqual")

_BinaryOp_less = (Core.Name "less")

_BinaryOp_lessEqual = (Core.Name "lessEqual")

_BinaryOp_greater = (Core.Name "greater")

_BinaryOp_greaterEqual = (Core.Name "greaterEqual")

_BinaryOp_add = (Core.Name "add")

_BinaryOp_subtract = (Core.Name "subtract")

_BinaryOp_bitwiseOr = (Core.Name "bitwiseOr")

_BinaryOp_bitwiseXor = (Core.Name "bitwiseXor")

_BinaryOp_multiply = (Core.Name "multiply")

_BinaryOp_divide = (Core.Name "divide")

_BinaryOp_remainder = (Core.Name "remainder")

_BinaryOp_leftShift = (Core.Name "leftShift")

_BinaryOp_rightShift = (Core.Name "rightShift")

_BinaryOp_bitwiseAnd = (Core.Name "bitwiseAnd")

_BinaryOp_bitClear = (Core.Name "bitClear")

data PrimaryExpr = 
  PrimaryExprOperand Operand |
  PrimaryExprConversion Conversion |
  PrimaryExprMethodExpr MethodExpr |
  PrimaryExprSelector SelectorExpr |
  PrimaryExprIndex IndexExpr |
  PrimaryExprSlice SliceExpr |
  PrimaryExprTypeAssertion TypeAssertionExpr |
  PrimaryExprCall CallExpr
  deriving (Eq, Ord, Read, Show)

_PrimaryExpr = (Core.Name "hydra.ext.go.syntax.PrimaryExpr")

_PrimaryExpr_operand = (Core.Name "operand")

_PrimaryExpr_conversion = (Core.Name "conversion")

_PrimaryExpr_methodExpr = (Core.Name "methodExpr")

_PrimaryExpr_selector = (Core.Name "selector")

_PrimaryExpr_index = (Core.Name "index")

_PrimaryExpr_slice = (Core.Name "slice")

_PrimaryExpr_typeAssertion = (Core.Name "typeAssertion")

_PrimaryExpr_call = (Core.Name "call")

data SelectorExpr = 
  SelectorExpr {
    selectorExprExpr :: PrimaryExpr,
    selectorExprSelector :: Identifier}
  deriving (Eq, Ord, Read, Show)

_SelectorExpr = (Core.Name "hydra.ext.go.syntax.SelectorExpr")

_SelectorExpr_expr = (Core.Name "expr")

_SelectorExpr_selector = (Core.Name "selector")

data IndexExpr = 
  IndexExpr {
    indexExprExpr :: PrimaryExpr,
    indexExprIndex :: Expression}
  deriving (Eq, Ord, Read, Show)

_IndexExpr = (Core.Name "hydra.ext.go.syntax.IndexExpr")

_IndexExpr_expr = (Core.Name "expr")

_IndexExpr_index = (Core.Name "index")

data SliceExpr = 
  SliceExpr {
    sliceExprExpr :: PrimaryExpr,
    sliceExprSlice :: Slice}
  deriving (Eq, Ord, Read, Show)

_SliceExpr = (Core.Name "hydra.ext.go.syntax.SliceExpr")

_SliceExpr_expr = (Core.Name "expr")

_SliceExpr_slice = (Core.Name "slice")

data TypeAssertionExpr = 
  TypeAssertionExpr {
    typeAssertionExprExpr :: PrimaryExpr,
    typeAssertionExprType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeAssertionExpr = (Core.Name "hydra.ext.go.syntax.TypeAssertionExpr")

_TypeAssertionExpr_expr = (Core.Name "expr")

_TypeAssertionExpr_type = (Core.Name "type")

data CallExpr = 
  CallExpr {
    callExprFunction :: PrimaryExpr,
    callExprArguments :: Arguments}
  deriving (Eq, Ord, Read, Show)

_CallExpr = (Core.Name "hydra.ext.go.syntax.CallExpr")

_CallExpr_function = (Core.Name "function")

_CallExpr_arguments = (Core.Name "arguments")

data Operand = 
  OperandLiteral Literal |
  OperandName_ OperandName |
  OperandParen Expression
  deriving (Eq, Ord, Read, Show)

_Operand = (Core.Name "hydra.ext.go.syntax.Operand")

_Operand_literal = (Core.Name "literal")

_Operand_name = (Core.Name "name")

_Operand_paren = (Core.Name "paren")

data OperandName = 
  OperandName {
    operandNameName :: QualifiedIdent,
    operandNameTypeArgs :: [Type]}
  deriving (Eq, Ord, Read, Show)

_OperandName = (Core.Name "hydra.ext.go.syntax.OperandName")

_OperandName_name = (Core.Name "name")

_OperandName_typeArgs = (Core.Name "typeArgs")

data Literal = 
  LiteralBasic BasicLit |
  LiteralComposite CompositeLit |
  LiteralFunction FunctionLit
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.go.syntax.Literal")

_Literal_basic = (Core.Name "basic")

_Literal_composite = (Core.Name "composite")

_Literal_function = (Core.Name "function")

data BasicLit = 
  BasicLitInt IntLit |
  BasicLitFloat FloatLit |
  BasicLitImaginary ImaginaryLit |
  BasicLitRune RuneLit |
  BasicLitString StringLit
  deriving (Eq, Ord, Read, Show)

_BasicLit = (Core.Name "hydra.ext.go.syntax.BasicLit")

_BasicLit_int = (Core.Name "int")

_BasicLit_float = (Core.Name "float")

_BasicLit_imaginary = (Core.Name "imaginary")

_BasicLit_rune = (Core.Name "rune")

_BasicLit_string = (Core.Name "string")

data CompositeLit = 
  CompositeLit {
    compositeLitType :: LiteralType,
    compositeLitValue :: LiteralValue}
  deriving (Eq, Ord, Read, Show)

_CompositeLit = (Core.Name "hydra.ext.go.syntax.CompositeLit")

_CompositeLit_type = (Core.Name "type")

_CompositeLit_value = (Core.Name "value")

data LiteralType = 
  LiteralTypeStruct StructType |
  LiteralTypeArray ArrayType |
  LiteralTypeInferredArray Type |
  LiteralTypeSlice SliceType |
  LiteralTypeMap MapType |
  LiteralTypeName TypeName
  deriving (Eq, Ord, Read, Show)

_LiteralType = (Core.Name "hydra.ext.go.syntax.LiteralType")

_LiteralType_struct = (Core.Name "struct")

_LiteralType_array = (Core.Name "array")

_LiteralType_inferredArray = (Core.Name "inferredArray")

_LiteralType_slice = (Core.Name "slice")

_LiteralType_map = (Core.Name "map")

_LiteralType_name = (Core.Name "name")

newtype LiteralValue = 
  LiteralValue {
    unLiteralValue :: [KeyedElement]}
  deriving (Eq, Ord, Read, Show)

_LiteralValue = (Core.Name "hydra.ext.go.syntax.LiteralValue")

newtype ElementList = 
  ElementList {
    unElementList :: [KeyedElement]}
  deriving (Eq, Ord, Read, Show)

_ElementList = (Core.Name "hydra.ext.go.syntax.ElementList")

data KeyedElement = 
  KeyedElement {
    keyedElementKey :: (Maybe Key),
    keyedElementElement :: Element}
  deriving (Eq, Ord, Read, Show)

_KeyedElement = (Core.Name "hydra.ext.go.syntax.KeyedElement")

_KeyedElement_key = (Core.Name "key")

_KeyedElement_element = (Core.Name "element")

data Key = 
  KeyField Identifier |
  KeyExpression Expression |
  KeyLiteral LiteralValue
  deriving (Eq, Ord, Read, Show)

_Key = (Core.Name "hydra.ext.go.syntax.Key")

_Key_field = (Core.Name "field")

_Key_expression = (Core.Name "expression")

_Key_literal = (Core.Name "literal")

data Element = 
  ElementExpression Expression |
  ElementLiteral LiteralValue
  deriving (Eq, Ord, Read, Show)

_Element = (Core.Name "hydra.ext.go.syntax.Element")

_Element_expression = (Core.Name "expression")

_Element_literal = (Core.Name "literal")

data FunctionLit = 
  FunctionLit {
    functionLitSignature :: Signature,
    functionLitBody :: FunctionBody}
  deriving (Eq, Ord, Read, Show)

_FunctionLit = (Core.Name "hydra.ext.go.syntax.FunctionLit")

_FunctionLit_signature = (Core.Name "signature")

_FunctionLit_body = (Core.Name "body")

newtype Selector = 
  Selector {
    unSelector :: Identifier}
  deriving (Eq, Ord, Read, Show)

_Selector = (Core.Name "hydra.ext.go.syntax.Selector")

newtype Index = 
  Index {
    unIndex :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_Index = (Core.Name "hydra.ext.go.syntax.Index")

data Slice = 
  SliceSimple SimpleSlice |
  SliceFull FullSlice
  deriving (Eq, Ord, Read, Show)

_Slice = (Core.Name "hydra.ext.go.syntax.Slice")

_Slice_simple = (Core.Name "simple")

_Slice_full = (Core.Name "full")

data SimpleSlice = 
  SimpleSlice {
    simpleSliceLow :: (Maybe Expression),
    simpleSliceHigh :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_SimpleSlice = (Core.Name "hydra.ext.go.syntax.SimpleSlice")

_SimpleSlice_low = (Core.Name "low")

_SimpleSlice_high = (Core.Name "high")

data FullSlice = 
  FullSlice {
    fullSliceLow :: (Maybe Expression),
    fullSliceHigh :: Expression,
    fullSliceMax :: Expression}
  deriving (Eq, Ord, Read, Show)

_FullSlice = (Core.Name "hydra.ext.go.syntax.FullSlice")

_FullSlice_low = (Core.Name "low")

_FullSlice_high = (Core.Name "high")

_FullSlice_max = (Core.Name "max")

newtype TypeAssertion = 
  TypeAssertion {
    unTypeAssertion :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeAssertion = (Core.Name "hydra.ext.go.syntax.TypeAssertion")

data Arguments = 
  Arguments {
    argumentsTypeArg :: (Maybe Type),
    argumentsExpressions :: [Expression],
    argumentsEllipsis :: Bool}
  deriving (Eq, Ord, Read, Show)

_Arguments = (Core.Name "hydra.ext.go.syntax.Arguments")

_Arguments_typeArg = (Core.Name "typeArg")

_Arguments_expressions = (Core.Name "expressions")

_Arguments_ellipsis = (Core.Name "ellipsis")

data MethodExpr = 
  MethodExpr {
    methodExprReceiver :: Type,
    methodExprMethod :: Identifier}
  deriving (Eq, Ord, Read, Show)

_MethodExpr = (Core.Name "hydra.ext.go.syntax.MethodExpr")

_MethodExpr_receiver = (Core.Name "receiver")

_MethodExpr_method = (Core.Name "method")

data Conversion = 
  Conversion {
    conversionType :: Type,
    conversionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_Conversion = (Core.Name "hydra.ext.go.syntax.Conversion")

_Conversion_type = (Core.Name "type")

_Conversion_expression = (Core.Name "expression")

data Statement = 
  StatementDeclaration Declaration |
  StatementLabeled LabeledStmt |
  StatementSimple SimpleStmt |
  StatementGo GoStmt |
  StatementReturn ReturnStmt |
  StatementBreak BreakStmt |
  StatementContinue ContinueStmt |
  StatementGoto GotoStmt |
  StatementFallthrough FallthroughStmt |
  StatementBlock Block |
  StatementIf IfStmt |
  StatementSwitch SwitchStmt |
  StatementSelect SelectStmt |
  StatementFor ForStmt |
  StatementDefer DeferStmt
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra.ext.go.syntax.Statement")

_Statement_declaration = (Core.Name "declaration")

_Statement_labeled = (Core.Name "labeled")

_Statement_simple = (Core.Name "simple")

_Statement_go = (Core.Name "go")

_Statement_return = (Core.Name "return")

_Statement_break = (Core.Name "break")

_Statement_continue = (Core.Name "continue")

_Statement_goto = (Core.Name "goto")

_Statement_fallthrough = (Core.Name "fallthrough")

_Statement_block = (Core.Name "block")

_Statement_if = (Core.Name "if")

_Statement_switch = (Core.Name "switch")

_Statement_select = (Core.Name "select")

_Statement_for = (Core.Name "for")

_Statement_defer = (Core.Name "defer")

data SimpleStmt = 
  SimpleStmtEmpty EmptyStmt |
  SimpleStmtExpression ExpressionStmt |
  SimpleStmtSend SendStmt |
  SimpleStmtIncDec IncDecStmt |
  SimpleStmtAssignment Assignment |
  SimpleStmtShortVarDecl ShortVarDecl
  deriving (Eq, Ord, Read, Show)

_SimpleStmt = (Core.Name "hydra.ext.go.syntax.SimpleStmt")

_SimpleStmt_empty = (Core.Name "empty")

_SimpleStmt_expression = (Core.Name "expression")

_SimpleStmt_send = (Core.Name "send")

_SimpleStmt_incDec = (Core.Name "incDec")

_SimpleStmt_assignment = (Core.Name "assignment")

_SimpleStmt_shortVarDecl = (Core.Name "shortVarDecl")

newtype EmptyStmt = 
  EmptyStmt {
    unEmptyStmt :: ()}
  deriving (Eq, Ord, Read, Show)

_EmptyStmt = (Core.Name "hydra.ext.go.syntax.EmptyStmt")

data LabeledStmt = 
  LabeledStmt {
    labeledStmtLabel :: Identifier,
    labeledStmtStatement :: Statement}
  deriving (Eq, Ord, Read, Show)

_LabeledStmt = (Core.Name "hydra.ext.go.syntax.LabeledStmt")

_LabeledStmt_label = (Core.Name "label")

_LabeledStmt_statement = (Core.Name "statement")

newtype ExpressionStmt = 
  ExpressionStmt {
    unExpressionStmt :: Expression}
  deriving (Eq, Ord, Read, Show)

_ExpressionStmt = (Core.Name "hydra.ext.go.syntax.ExpressionStmt")

data SendStmt = 
  SendStmt {
    sendStmtChannel :: Expression,
    sendStmtValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_SendStmt = (Core.Name "hydra.ext.go.syntax.SendStmt")

_SendStmt_channel = (Core.Name "channel")

_SendStmt_value = (Core.Name "value")

data IncDecStmt = 
  IncDecStmt {
    incDecStmtExpression :: Expression,
    incDecStmtIncrement :: Bool}
  deriving (Eq, Ord, Read, Show)

_IncDecStmt = (Core.Name "hydra.ext.go.syntax.IncDecStmt")

_IncDecStmt_expression = (Core.Name "expression")

_IncDecStmt_increment = (Core.Name "increment")

data Assignment = 
  Assignment {
    assignmentLhs :: [Expression],
    assignmentOp :: AssignOp,
    assignmentRhs :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_Assignment = (Core.Name "hydra.ext.go.syntax.Assignment")

_Assignment_lhs = (Core.Name "lhs")

_Assignment_op = (Core.Name "op")

_Assignment_rhs = (Core.Name "rhs")

data AssignOp = 
  AssignOpSimple  |
  AssignOpAdd AddOp |
  AssignOpMul MulOp
  deriving (Eq, Ord, Read, Show)

_AssignOp = (Core.Name "hydra.ext.go.syntax.AssignOp")

_AssignOp_simple = (Core.Name "simple")

_AssignOp_add = (Core.Name "add")

_AssignOp_mul = (Core.Name "mul")

data IfStmt = 
  IfStmt {
    ifStmtInit :: (Maybe SimpleStmt),
    ifStmtCondition :: Expression,
    ifStmtThen :: Block,
    ifStmtElse :: (Maybe ElseClause)}
  deriving (Eq, Ord, Read, Show)

_IfStmt = (Core.Name "hydra.ext.go.syntax.IfStmt")

_IfStmt_init = (Core.Name "init")

_IfStmt_condition = (Core.Name "condition")

_IfStmt_then = (Core.Name "then")

_IfStmt_else = (Core.Name "else")

data ElseClause = 
  ElseClauseIf IfStmt |
  ElseClauseBlock Block
  deriving (Eq, Ord, Read, Show)

_ElseClause = (Core.Name "hydra.ext.go.syntax.ElseClause")

_ElseClause_if = (Core.Name "if")

_ElseClause_block = (Core.Name "block")

data SwitchStmt = 
  SwitchStmtExpression ExprSwitchStmt |
  SwitchStmtType TypeSwitchStmt
  deriving (Eq, Ord, Read, Show)

_SwitchStmt = (Core.Name "hydra.ext.go.syntax.SwitchStmt")

_SwitchStmt_expression = (Core.Name "expression")

_SwitchStmt_type = (Core.Name "type")

data ExprSwitchStmt = 
  ExprSwitchStmt {
    exprSwitchStmtInit :: (Maybe SimpleStmt),
    exprSwitchStmtExpression :: (Maybe Expression),
    exprSwitchStmtCases :: [ExprCaseClause]}
  deriving (Eq, Ord, Read, Show)

_ExprSwitchStmt = (Core.Name "hydra.ext.go.syntax.ExprSwitchStmt")

_ExprSwitchStmt_init = (Core.Name "init")

_ExprSwitchStmt_expression = (Core.Name "expression")

_ExprSwitchStmt_cases = (Core.Name "cases")

data ExprCaseClause = 
  ExprCaseClause {
    exprCaseClauseCase :: (Maybe [Expression]),
    exprCaseClauseStatements :: [Statement]}
  deriving (Eq, Ord, Read, Show)

_ExprCaseClause = (Core.Name "hydra.ext.go.syntax.ExprCaseClause")

_ExprCaseClause_case = (Core.Name "case")

_ExprCaseClause_statements = (Core.Name "statements")

data TypeSwitchStmt = 
  TypeSwitchStmt {
    typeSwitchStmtInit :: (Maybe SimpleStmt),
    typeSwitchStmtGuard :: TypeSwitchGuard,
    typeSwitchStmtCases :: [TypeCaseClause]}
  deriving (Eq, Ord, Read, Show)

_TypeSwitchStmt = (Core.Name "hydra.ext.go.syntax.TypeSwitchStmt")

_TypeSwitchStmt_init = (Core.Name "init")

_TypeSwitchStmt_guard = (Core.Name "guard")

_TypeSwitchStmt_cases = (Core.Name "cases")

data TypeSwitchGuard = 
  TypeSwitchGuard {
    typeSwitchGuardName :: (Maybe Identifier),
    typeSwitchGuardExpression :: PrimaryExpr}
  deriving (Eq, Ord, Read, Show)

_TypeSwitchGuard = (Core.Name "hydra.ext.go.syntax.TypeSwitchGuard")

_TypeSwitchGuard_name = (Core.Name "name")

_TypeSwitchGuard_expression = (Core.Name "expression")

data TypeCaseClause = 
  TypeCaseClause {
    typeCaseClauseCase :: (Maybe [Type]),
    typeCaseClauseStatements :: [Statement]}
  deriving (Eq, Ord, Read, Show)

_TypeCaseClause = (Core.Name "hydra.ext.go.syntax.TypeCaseClause")

_TypeCaseClause_case = (Core.Name "case")

_TypeCaseClause_statements = (Core.Name "statements")

data ForStmt = 
  ForStmt {
    forStmtClause :: (Maybe ForClauseOrRange),
    forStmtBody :: Block}
  deriving (Eq, Ord, Read, Show)

_ForStmt = (Core.Name "hydra.ext.go.syntax.ForStmt")

_ForStmt_clause = (Core.Name "clause")

_ForStmt_body = (Core.Name "body")

data ForClauseOrRange = 
  ForClauseOrRangeCondition Expression |
  ForClauseOrRangeClause ForClause |
  ForClauseOrRangeRange RangeClause
  deriving (Eq, Ord, Read, Show)

_ForClauseOrRange = (Core.Name "hydra.ext.go.syntax.ForClauseOrRange")

_ForClauseOrRange_condition = (Core.Name "condition")

_ForClauseOrRange_clause = (Core.Name "clause")

_ForClauseOrRange_range = (Core.Name "range")

data ForClause = 
  ForClause {
    forClauseInit :: (Maybe SimpleStmt),
    forClauseCondition :: (Maybe Expression),
    forClausePost :: (Maybe SimpleStmt)}
  deriving (Eq, Ord, Read, Show)

_ForClause = (Core.Name "hydra.ext.go.syntax.ForClause")

_ForClause_init = (Core.Name "init")

_ForClause_condition = (Core.Name "condition")

_ForClause_post = (Core.Name "post")

data RangeClause = 
  RangeClause {
    rangeClauseVars :: (Maybe RangeVars),
    rangeClauseExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_RangeClause = (Core.Name "hydra.ext.go.syntax.RangeClause")

_RangeClause_vars = (Core.Name "vars")

_RangeClause_expression = (Core.Name "expression")

data RangeVars = 
  RangeVarsAssign [Expression] |
  RangeVarsDeclare [Identifier]
  deriving (Eq, Ord, Read, Show)

_RangeVars = (Core.Name "hydra.ext.go.syntax.RangeVars")

_RangeVars_assign = (Core.Name "assign")

_RangeVars_declare = (Core.Name "declare")

newtype GoStmt = 
  GoStmt {
    unGoStmt :: Expression}
  deriving (Eq, Ord, Read, Show)

_GoStmt = (Core.Name "hydra.ext.go.syntax.GoStmt")

newtype SelectStmt = 
  SelectStmt {
    unSelectStmt :: [CommClause]}
  deriving (Eq, Ord, Read, Show)

_SelectStmt = (Core.Name "hydra.ext.go.syntax.SelectStmt")

data CommClause = 
  CommClause {
    commClauseCase :: CommCase,
    commClauseStatements :: [Statement]}
  deriving (Eq, Ord, Read, Show)

_CommClause = (Core.Name "hydra.ext.go.syntax.CommClause")

_CommClause_case = (Core.Name "case")

_CommClause_statements = (Core.Name "statements")

data CommCase = 
  CommCaseSend SendStmt |
  CommCaseReceive ReceiveCase |
  CommCaseDefault 
  deriving (Eq, Ord, Read, Show)

_CommCase = (Core.Name "hydra.ext.go.syntax.CommCase")

_CommCase_send = (Core.Name "send")

_CommCase_receive = (Core.Name "receive")

_CommCase_default = (Core.Name "default")

data ReceiveCase = 
  ReceiveCase {
    receiveCaseVars :: (Maybe RangeVars),
    receiveCaseExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_ReceiveCase = (Core.Name "hydra.ext.go.syntax.ReceiveCase")

_ReceiveCase_vars = (Core.Name "vars")

_ReceiveCase_expression = (Core.Name "expression")

newtype ReturnStmt = 
  ReturnStmt {
    unReturnStmt :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_ReturnStmt = (Core.Name "hydra.ext.go.syntax.ReturnStmt")

newtype BreakStmt = 
  BreakStmt {
    unBreakStmt :: (Maybe Identifier)}
  deriving (Eq, Ord, Read, Show)

_BreakStmt = (Core.Name "hydra.ext.go.syntax.BreakStmt")

newtype ContinueStmt = 
  ContinueStmt {
    unContinueStmt :: (Maybe Identifier)}
  deriving (Eq, Ord, Read, Show)

_ContinueStmt = (Core.Name "hydra.ext.go.syntax.ContinueStmt")

newtype GotoStmt = 
  GotoStmt {
    unGotoStmt :: Identifier}
  deriving (Eq, Ord, Read, Show)

_GotoStmt = (Core.Name "hydra.ext.go.syntax.GotoStmt")

newtype FallthroughStmt = 
  FallthroughStmt {
    unFallthroughStmt :: ()}
  deriving (Eq, Ord, Read, Show)

_FallthroughStmt = (Core.Name "hydra.ext.go.syntax.FallthroughStmt")

newtype DeferStmt = 
  DeferStmt {
    unDeferStmt :: Expression}
  deriving (Eq, Ord, Read, Show)

_DeferStmt = (Core.Name "hydra.ext.go.syntax.DeferStmt")

newtype Block = 
  Block {
    unBlock :: [Statement]}
  deriving (Eq, Ord, Read, Show)

_Block = (Core.Name "hydra.ext.go.syntax.Block")

data UnaryOp = 
  UnaryOpPlus  |
  UnaryOpMinus  |
  UnaryOpNot  |
  UnaryOpXor  |
  UnaryOpDeref  |
  UnaryOpAddressOf  |
  UnaryOpReceive 
  deriving (Eq, Ord, Read, Show)

_UnaryOp = (Core.Name "hydra.ext.go.syntax.UnaryOp")

_UnaryOp_plus = (Core.Name "plus")

_UnaryOp_minus = (Core.Name "minus")

_UnaryOp_not = (Core.Name "not")

_UnaryOp_xor = (Core.Name "xor")

_UnaryOp_deref = (Core.Name "deref")

_UnaryOp_addressOf = (Core.Name "addressOf")

_UnaryOp_receive = (Core.Name "receive")

data MulOp = 
  MulOpMultiply  |
  MulOpDivide  |
  MulOpRemainder  |
  MulOpLeftShift  |
  MulOpRightShift  |
  MulOpBitwiseAnd  |
  MulOpBitClear 
  deriving (Eq, Ord, Read, Show)

_MulOp = (Core.Name "hydra.ext.go.syntax.MulOp")

_MulOp_multiply = (Core.Name "multiply")

_MulOp_divide = (Core.Name "divide")

_MulOp_remainder = (Core.Name "remainder")

_MulOp_leftShift = (Core.Name "leftShift")

_MulOp_rightShift = (Core.Name "rightShift")

_MulOp_bitwiseAnd = (Core.Name "bitwiseAnd")

_MulOp_bitClear = (Core.Name "bitClear")

data AddOp = 
  AddOpAdd  |
  AddOpSubtract  |
  AddOpBitwiseOr  |
  AddOpBitwiseXor 
  deriving (Eq, Ord, Read, Show)

_AddOp = (Core.Name "hydra.ext.go.syntax.AddOp")

_AddOp_add = (Core.Name "add")

_AddOp_subtract = (Core.Name "subtract")

_AddOp_bitwiseOr = (Core.Name "bitwiseOr")

_AddOp_bitwiseXor = (Core.Name "bitwiseXor")

data RelOp = 
  RelOpEqual  |
  RelOpNotEqual  |
  RelOpLess  |
  RelOpLessEqual  |
  RelOpGreater  |
  RelOpGreaterEqual 
  deriving (Eq, Ord, Read, Show)

_RelOp = (Core.Name "hydra.ext.go.syntax.RelOp")

_RelOp_equal = (Core.Name "equal")

_RelOp_notEqual = (Core.Name "notEqual")

_RelOp_less = (Core.Name "less")

_RelOp_lessEqual = (Core.Name "lessEqual")

_RelOp_greater = (Core.Name "greater")

_RelOp_greaterEqual = (Core.Name "greaterEqual")
