-- | A Haskell syntax model, loosely based on Language.Haskell.Tools.AST

module Hydra.Ext.Haskell.Ast where

import qualified Hydra.Core as Core
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A pattern-matching alternative
data Alternative = 
  Alternative {
    alternativePattern :: Pattern,
    alternativeRhs :: CaseRhs,
    alternativeBinds :: (Maybe LocalBindings)}
  deriving (Eq, Ord, Read, Show)

_Alternative = (Core.Name "hydra.ext.haskell.ast.Alternative")

_Alternative_pattern = (Core.Name "pattern")

_Alternative_rhs = (Core.Name "rhs")

_Alternative_binds = (Core.Name "binds")

-- | A type assertion
data Assertion = 
  AssertionClass ClassAssertion |
  AssertionTuple [Assertion]
  deriving (Eq, Ord, Read, Show)

_Assertion = (Core.Name "hydra.ext.haskell.ast.Assertion")

_Assertion_class = (Core.Name "class")

_Assertion_tuple = (Core.Name "tuple")

data ClassAssertion = 
  ClassAssertion {
    classAssertionName :: Name,
    classAssertionTypes :: [Type]}
  deriving (Eq, Ord, Read, Show)

_ClassAssertion = (Core.Name "hydra.ext.haskell.ast.ClassAssertion")

_ClassAssertion_name = (Core.Name "name")

_ClassAssertion_types = (Core.Name "types")

-- | The right-hand side of a pattern-matching alternative
newtype CaseRhs = 
  CaseRhs {
    unCaseRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_CaseRhs = (Core.Name "hydra.ext.haskell.ast.CaseRhs")

-- | A data constructor
data Constructor = 
  ConstructorOrdinary OrdinaryConstructor |
  ConstructorRecord RecordConstructor
  deriving (Eq, Ord, Read, Show)

_Constructor = (Core.Name "hydra.ext.haskell.ast.Constructor")

_Constructor_ordinary = (Core.Name "ordinary")

_Constructor_record = (Core.Name "record")

-- | An ordinary (positional) data constructor
data OrdinaryConstructor = 
  OrdinaryConstructor {
    ordinaryConstructorName :: Name,
    ordinaryConstructorFields :: [Type]}
  deriving (Eq, Ord, Read, Show)

_OrdinaryConstructor = (Core.Name "hydra.ext.haskell.ast.OrdinaryConstructor")

_OrdinaryConstructor_name = (Core.Name "name")

_OrdinaryConstructor_fields = (Core.Name "fields")

-- | A record-style data constructor
data RecordConstructor = 
  RecordConstructor {
    recordConstructorName :: Name,
    recordConstructorFields :: [FieldWithComments]}
  deriving (Eq, Ord, Read, Show)

_RecordConstructor = (Core.Name "hydra.ext.haskell.ast.RecordConstructor")

_RecordConstructor_name = (Core.Name "name")

_RecordConstructor_fields = (Core.Name "fields")

-- | A data constructor together with any comments
data ConstructorWithComments = 
  ConstructorWithComments {
    constructorWithCommentsBody :: Constructor,
    constructorWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_ConstructorWithComments = (Core.Name "hydra.ext.haskell.ast.ConstructorWithComments")

_ConstructorWithComments_body = (Core.Name "body")

_ConstructorWithComments_comments = (Core.Name "comments")

-- | A data type declaration
data DataDeclaration = 
  DataDeclaration {
    dataDeclarationKeyword :: DataOrNewtype,
    dataDeclarationContext :: [Assertion],
    dataDeclarationHead :: DeclarationHead,
    dataDeclarationConstructors :: [ConstructorWithComments],
    dataDeclarationDeriving :: [Deriving]}
  deriving (Eq, Ord, Read, Show)

_DataDeclaration = (Core.Name "hydra.ext.haskell.ast.DataDeclaration")

_DataDeclaration_keyword = (Core.Name "keyword")

_DataDeclaration_context = (Core.Name "context")

_DataDeclaration_head = (Core.Name "head")

_DataDeclaration_constructors = (Core.Name "constructors")

_DataDeclaration_deriving = (Core.Name "deriving")

-- | The 'data' versus 'newtype keyword
data DataOrNewtype = 
  DataOrNewtypeData  |
  DataOrNewtypeNewtype 
  deriving (Eq, Ord, Read, Show)

_DataOrNewtype = (Core.Name "hydra.ext.haskell.ast.DataOrNewtype")

_DataOrNewtype_data = (Core.Name "data")

_DataOrNewtype_newtype = (Core.Name "newtype")

-- | A data declaration together with any comments
data DeclarationWithComments = 
  DeclarationWithComments {
    declarationWithCommentsBody :: Declaration,
    declarationWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_DeclarationWithComments = (Core.Name "hydra.ext.haskell.ast.DeclarationWithComments")

_DeclarationWithComments_body = (Core.Name "body")

_DeclarationWithComments_comments = (Core.Name "comments")

-- | A data or value declaration
data Declaration = 
  DeclarationData DataDeclaration |
  DeclarationType TypeDeclaration |
  DeclarationValueBinding ValueBinding |
  DeclarationTypedBinding TypedBinding
  deriving (Eq, Ord, Read, Show)

_Declaration = (Core.Name "hydra.ext.haskell.ast.Declaration")

_Declaration_data = (Core.Name "data")

_Declaration_type = (Core.Name "type")

_Declaration_valueBinding = (Core.Name "valueBinding")

_Declaration_typedBinding = (Core.Name "typedBinding")

-- | The left-hand side of a declaration
data DeclarationHead = 
  DeclarationHeadApplication ApplicationDeclarationHead |
  DeclarationHeadParens DeclarationHead |
  DeclarationHeadSimple Name
  deriving (Eq, Ord, Read, Show)

_DeclarationHead = (Core.Name "hydra.ext.haskell.ast.DeclarationHead")

_DeclarationHead_application = (Core.Name "application")

_DeclarationHead_parens = (Core.Name "parens")

_DeclarationHead_simple = (Core.Name "simple")

-- | An application-style declaration head
data ApplicationDeclarationHead = 
  ApplicationDeclarationHead {
    applicationDeclarationHeadFunction :: DeclarationHead,
    applicationDeclarationHeadOperand :: Variable}
  deriving (Eq, Ord, Read, Show)

_ApplicationDeclarationHead = (Core.Name "hydra.ext.haskell.ast.ApplicationDeclarationHead")

_ApplicationDeclarationHead_function = (Core.Name "function")

_ApplicationDeclarationHead_operand = (Core.Name "operand")

-- | A 'deriving' statement
newtype Deriving = 
  Deriving {
    unDeriving :: [Name]}
  deriving (Eq, Ord, Read, Show)

_Deriving = (Core.Name "hydra.ext.haskell.ast.Deriving")

-- | An export statement
data Export = 
  ExportDeclaration ImportExportSpec |
  ExportModule ModuleName
  deriving (Eq, Ord, Read, Show)

_Export = (Core.Name "hydra.ext.haskell.ast.Export")

_Export_declaration = (Core.Name "declaration")

_Export_module = (Core.Name "module")

-- | A data expression
data Expression = 
  ExpressionApplication ApplicationExpression |
  ExpressionCase CaseExpression |
  ExpressionConstructRecord ConstructRecordExpression |
  ExpressionDo [Statement] |
  ExpressionIf IfExpression |
  ExpressionInfixApplication InfixApplicationExpression |
  ExpressionLiteral Literal |
  ExpressionLambda LambdaExpression |
  ExpressionLeftSection SectionExpression |
  ExpressionLet LetExpression |
  ExpressionList [Expression] |
  ExpressionParens Expression |
  ExpressionPrefixApplication PrefixApplicationExpression |
  ExpressionRightSection SectionExpression |
  ExpressionTuple [Expression] |
  ExpressionTypeSignature TypeSignatureExpression |
  ExpressionUpdateRecord UpdateRecordExpression |
  ExpressionVariable Name
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra.ext.haskell.ast.Expression")

_Expression_application = (Core.Name "application")

_Expression_case = (Core.Name "case")

_Expression_constructRecord = (Core.Name "constructRecord")

_Expression_do = (Core.Name "do")

_Expression_if = (Core.Name "if")

_Expression_infixApplication = (Core.Name "infixApplication")

_Expression_literal = (Core.Name "literal")

_Expression_lambda = (Core.Name "lambda")

_Expression_leftSection = (Core.Name "leftSection")

_Expression_let = (Core.Name "let")

_Expression_list = (Core.Name "list")

_Expression_parens = (Core.Name "parens")

_Expression_prefixApplication = (Core.Name "prefixApplication")

_Expression_rightSection = (Core.Name "rightSection")

_Expression_tuple = (Core.Name "tuple")

_Expression_typeSignature = (Core.Name "typeSignature")

_Expression_updateRecord = (Core.Name "updateRecord")

_Expression_variable = (Core.Name "variable")

-- | An application expression
data ApplicationExpression = 
  ApplicationExpression {
    applicationExpressionFunction :: Expression,
    applicationExpressionArgument :: Expression}
  deriving (Eq, Ord, Read, Show)

_ApplicationExpression = (Core.Name "hydra.ext.haskell.ast.ApplicationExpression")

_ApplicationExpression_function = (Core.Name "function")

_ApplicationExpression_argument = (Core.Name "argument")

-- | A case expression
data CaseExpression = 
  CaseExpression {
    caseExpressionCase :: Expression,
    caseExpressionAlternatives :: [Alternative]}
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra.ext.haskell.ast.CaseExpression")

_CaseExpression_case = (Core.Name "case")

_CaseExpression_alternatives = (Core.Name "alternatives")

-- | A record constructor expression
data ConstructRecordExpression = 
  ConstructRecordExpression {
    constructRecordExpressionName :: Name,
    constructRecordExpressionFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)

_ConstructRecordExpression = (Core.Name "hydra.ext.haskell.ast.ConstructRecordExpression")

_ConstructRecordExpression_name = (Core.Name "name")

_ConstructRecordExpression_fields = (Core.Name "fields")

-- | An 'if' expression
data IfExpression = 
  IfExpression {
    ifExpressionCondition :: Expression,
    ifExpressionThen :: Expression,
    ifExpressionElse :: Expression}
  deriving (Eq, Ord, Read, Show)

_IfExpression = (Core.Name "hydra.ext.haskell.ast.IfExpression")

_IfExpression_condition = (Core.Name "condition")

_IfExpression_then = (Core.Name "then")

_IfExpression_else = (Core.Name "else")

-- | An infix application expression
data InfixApplicationExpression = 
  InfixApplicationExpression {
    infixApplicationExpressionLhs :: Expression,
    infixApplicationExpressionOperator :: Operator,
    infixApplicationExpressionRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_InfixApplicationExpression = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression")

_InfixApplicationExpression_lhs = (Core.Name "lhs")

_InfixApplicationExpression_operator = (Core.Name "operator")

_InfixApplicationExpression_rhs = (Core.Name "rhs")

-- | A lambda expression
data LambdaExpression = 
  LambdaExpression {
    lambdaExpressionBindings :: [Pattern],
    lambdaExpressionInner :: Expression}
  deriving (Eq, Ord, Read, Show)

_LambdaExpression = (Core.Name "hydra.ext.haskell.ast.LambdaExpression")

_LambdaExpression_bindings = (Core.Name "bindings")

_LambdaExpression_inner = (Core.Name "inner")

-- | A 'let' expression
data LetExpression = 
  LetExpression {
    letExpressionBindings :: [LocalBinding],
    letExpressionInner :: Expression}
  deriving (Eq, Ord, Read, Show)

_LetExpression = (Core.Name "hydra.ext.haskell.ast.LetExpression")

_LetExpression_bindings = (Core.Name "bindings")

_LetExpression_inner = (Core.Name "inner")

-- | A prefix expression
data PrefixApplicationExpression = 
  PrefixApplicationExpression {
    prefixApplicationExpressionOperator :: Operator,
    prefixApplicationExpressionRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_PrefixApplicationExpression = (Core.Name "hydra.ext.haskell.ast.PrefixApplicationExpression")

_PrefixApplicationExpression_operator = (Core.Name "operator")

_PrefixApplicationExpression_rhs = (Core.Name "rhs")

-- | A section expression
data SectionExpression = 
  SectionExpression {
    sectionExpressionOperator :: Operator,
    sectionExpressionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_SectionExpression = (Core.Name "hydra.ext.haskell.ast.SectionExpression")

_SectionExpression_operator = (Core.Name "operator")

_SectionExpression_expression = (Core.Name "expression")

-- | A type signature expression
data TypeSignatureExpression = 
  TypeSignatureExpression {
    typeSignatureExpressionInner :: Expression,
    typeSignatureExpressionType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeSignatureExpression = (Core.Name "hydra.ext.haskell.ast.TypeSignatureExpression")

_TypeSignatureExpression_inner = (Core.Name "inner")

_TypeSignatureExpression_type = (Core.Name "type")

-- | An update record expression
data UpdateRecordExpression = 
  UpdateRecordExpression {
    updateRecordExpressionInner :: Expression,
    updateRecordExpressionFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)

_UpdateRecordExpression = (Core.Name "hydra.ext.haskell.ast.UpdateRecordExpression")

_UpdateRecordExpression_inner = (Core.Name "inner")

_UpdateRecordExpression_fields = (Core.Name "fields")

-- | A field (name/type pair)
data Field = 
  Field {
    fieldName :: Name,
    fieldType :: Type}
  deriving (Eq, Ord, Read, Show)

_Field = (Core.Name "hydra.ext.haskell.ast.Field")

_Field_name = (Core.Name "name")

_Field_type = (Core.Name "type")

-- | A field together with any comments
data FieldWithComments = 
  FieldWithComments {
    fieldWithCommentsField :: Field,
    fieldWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_FieldWithComments = (Core.Name "hydra.ext.haskell.ast.FieldWithComments")

_FieldWithComments_field = (Core.Name "field")

_FieldWithComments_comments = (Core.Name "comments")

-- | A field name and value
data FieldUpdate = 
  FieldUpdate {
    fieldUpdateName :: Name,
    fieldUpdateValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_FieldUpdate = (Core.Name "hydra.ext.haskell.ast.FieldUpdate")

_FieldUpdate_name = (Core.Name "name")

_FieldUpdate_value = (Core.Name "value")

-- | An import statement
data Import = 
  Import {
    importQualified :: Bool,
    importModule :: ModuleName,
    importAs :: (Maybe ModuleName),
    importSpec :: (Maybe SpecImport)}
  deriving (Eq, Ord, Read, Show)

_Import = (Core.Name "hydra.ext.haskell.ast.Import")

_Import_qualified = (Core.Name "qualified")

_Import_module = (Core.Name "module")

_Import_as = (Core.Name "as")

_Import_spec = (Core.Name "spec")

-- | An import specification
data SpecImport = 
  SpecImportList [ImportExportSpec] |
  SpecImportHiding [ImportExportSpec]
  deriving (Eq, Ord, Read, Show)

_SpecImport = (Core.Name "hydra.ext.haskell.ast.SpecImport")

_SpecImport_list = (Core.Name "list")

_SpecImport_hiding = (Core.Name "hiding")

-- | An import modifier ('pattern' or 'type')
data ImportModifier = 
  ImportModifierPattern  |
  ImportModifierType 
  deriving (Eq, Ord, Read, Show)

_ImportModifier = (Core.Name "hydra.ext.haskell.ast.ImportModifier")

_ImportModifier_pattern = (Core.Name "pattern")

_ImportModifier_type = (Core.Name "type")

-- | An import or export specification
data ImportExportSpec = 
  ImportExportSpec {
    importExportSpecModifier :: (Maybe ImportModifier),
    importExportSpecName :: Name,
    importExportSpecSubspec :: (Maybe SubspecImportExportSpec)}
  deriving (Eq, Ord, Read, Show)

_ImportExportSpec = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec")

_ImportExportSpec_modifier = (Core.Name "modifier")

_ImportExportSpec_name = (Core.Name "name")

_ImportExportSpec_subspec = (Core.Name "subspec")

data SubspecImportExportSpec = 
  SubspecImportExportSpecAll  |
  SubspecImportExportSpecList [Name]
  deriving (Eq, Ord, Read, Show)

_SubspecImportExportSpec = (Core.Name "hydra.ext.haskell.ast.SubspecImportExportSpec")

_SubspecImportExportSpec_all = (Core.Name "all")

_SubspecImportExportSpec_list = (Core.Name "list")

-- | A literal value
data Literal = 
  LiteralChar Int |
  LiteralDouble Double |
  LiteralFloat Float |
  LiteralInt Int |
  LiteralInteger Integer |
  LiteralString String
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.haskell.ast.Literal")

_Literal_char = (Core.Name "char")

_Literal_double = (Core.Name "double")

_Literal_float = (Core.Name "float")

_Literal_int = (Core.Name "int")

_Literal_integer = (Core.Name "integer")

_Literal_string = (Core.Name "string")

data LocalBinding = 
  LocalBindingSignature TypeSignature |
  LocalBindingValue ValueBinding
  deriving (Eq, Ord, Read, Show)

_LocalBinding = (Core.Name "hydra.ext.haskell.ast.LocalBinding")

_LocalBinding_signature = (Core.Name "signature")

_LocalBinding_value = (Core.Name "value")

newtype LocalBindings = 
  LocalBindings {
    unLocalBindings :: [LocalBinding]}
  deriving (Eq, Ord, Read, Show)

_LocalBindings = (Core.Name "hydra.ext.haskell.ast.LocalBindings")

data Module = 
  Module {
    moduleHead :: (Maybe ModuleHead),
    moduleImports :: [Import],
    moduleDeclarations :: [DeclarationWithComments]}
  deriving (Eq, Ord, Read, Show)

_Module = (Core.Name "hydra.ext.haskell.ast.Module")

_Module_head = (Core.Name "head")

_Module_imports = (Core.Name "imports")

_Module_declarations = (Core.Name "declarations")

data ModuleHead = 
  ModuleHead {
    moduleHeadComments :: (Maybe String),
    moduleHeadName :: ModuleName,
    moduleHeadExports :: [Export]}
  deriving (Eq, Ord, Read, Show)

_ModuleHead = (Core.Name "hydra.ext.haskell.ast.ModuleHead")

_ModuleHead_comments = (Core.Name "comments")

_ModuleHead_name = (Core.Name "name")

_ModuleHead_exports = (Core.Name "exports")

newtype ModuleName = 
  ModuleName {
    unModuleName :: String}
  deriving (Eq, Ord, Read, Show)

_ModuleName = (Core.Name "hydra.ext.haskell.ast.ModuleName")

data Name = 
  NameImplicit QualifiedName |
  NameNormal QualifiedName |
  NameParens QualifiedName
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra.ext.haskell.ast.Name")

_Name_implicit = (Core.Name "implicit")

_Name_normal = (Core.Name "normal")

_Name_parens = (Core.Name "parens")

newtype NamePart = 
  NamePart {
    unNamePart :: String}
  deriving (Eq, Ord, Read, Show)

_NamePart = (Core.Name "hydra.ext.haskell.ast.NamePart")

data Operator = 
  OperatorBacktick QualifiedName |
  OperatorNormal QualifiedName
  deriving (Eq, Ord, Read, Show)

_Operator = (Core.Name "hydra.ext.haskell.ast.Operator")

_Operator_backtick = (Core.Name "backtick")

_Operator_normal = (Core.Name "normal")

data Pattern = 
  PatternApplication ApplicationPattern |
  PatternAs AsPattern |
  PatternList [Pattern] |
  PatternLiteral Literal |
  PatternName Name |
  PatternParens Pattern |
  PatternRecord RecordPattern |
  PatternTuple [Pattern] |
  PatternTyped TypedPattern |
  PatternWildcard 
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra.ext.haskell.ast.Pattern")

_Pattern_application = (Core.Name "application")

_Pattern_as = (Core.Name "as")

_Pattern_list = (Core.Name "list")

_Pattern_literal = (Core.Name "literal")

_Pattern_name = (Core.Name "name")

_Pattern_parens = (Core.Name "parens")

_Pattern_record = (Core.Name "record")

_Pattern_tuple = (Core.Name "tuple")

_Pattern_typed = (Core.Name "typed")

_Pattern_wildcard = (Core.Name "wildcard")

data ApplicationPattern = 
  ApplicationPattern {
    applicationPatternName :: Name,
    applicationPatternArgs :: [Pattern]}
  deriving (Eq, Ord, Read, Show)

_ApplicationPattern = (Core.Name "hydra.ext.haskell.ast.ApplicationPattern")

_ApplicationPattern_name = (Core.Name "name")

_ApplicationPattern_args = (Core.Name "args")

data AsPattern = 
  AsPattern {
    asPatternName :: Name,
    asPatternInner :: Pattern}
  deriving (Eq, Ord, Read, Show)

_AsPattern = (Core.Name "hydra.ext.haskell.ast.AsPattern")

_AsPattern_name = (Core.Name "name")

_AsPattern_inner = (Core.Name "inner")

data RecordPattern = 
  RecordPattern {
    recordPatternName :: Name,
    recordPatternFields :: [PatternField]}
  deriving (Eq, Ord, Read, Show)

_RecordPattern = (Core.Name "hydra.ext.haskell.ast.RecordPattern")

_RecordPattern_name = (Core.Name "name")

_RecordPattern_fields = (Core.Name "fields")

data TypedPattern = 
  TypedPattern {
    typedPatternInner :: Pattern,
    typedPatternType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypedPattern = (Core.Name "hydra.ext.haskell.ast.TypedPattern")

_TypedPattern_inner = (Core.Name "inner")

_TypedPattern_type = (Core.Name "type")

data PatternField = 
  PatternField {
    patternFieldName :: Name,
    patternFieldPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_PatternField = (Core.Name "hydra.ext.haskell.ast.PatternField")

_PatternField_name = (Core.Name "name")

_PatternField_pattern = (Core.Name "pattern")

data QualifiedName = 
  QualifiedName {
    qualifiedNameQualifiers :: [NamePart],
    qualifiedNameUnqualified :: NamePart}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra.ext.haskell.ast.QualifiedName")

_QualifiedName_qualifiers = (Core.Name "qualifiers")

_QualifiedName_unqualified = (Core.Name "unqualified")

newtype RightHandSide = 
  RightHandSide {
    unRightHandSide :: Expression}
  deriving (Eq, Ord, Read, Show)

_RightHandSide = (Core.Name "hydra.ext.haskell.ast.RightHandSide")

newtype Statement = 
  Statement {
    unStatement :: Expression}
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra.ext.haskell.ast.Statement")

data Type = 
  TypeApplication ApplicationType |
  TypeCtx ContextType |
  TypeFunction FunctionType |
  TypeInfix InfixType |
  TypeList Type |
  TypeParens Type |
  TypeTuple [Type] |
  TypeVariable Name
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra.ext.haskell.ast.Type")

_Type_application = (Core.Name "application")

_Type_ctx = (Core.Name "ctx")

_Type_function = (Core.Name "function")

_Type_infix = (Core.Name "infix")

_Type_list = (Core.Name "list")

_Type_parens = (Core.Name "parens")

_Type_tuple = (Core.Name "tuple")

_Type_variable = (Core.Name "variable")

data ApplicationType = 
  ApplicationType {
    applicationTypeContext :: Type,
    applicationTypeArgument :: Type}
  deriving (Eq, Ord, Read, Show)

_ApplicationType = (Core.Name "hydra.ext.haskell.ast.ApplicationType")

_ApplicationType_context = (Core.Name "context")

_ApplicationType_argument = (Core.Name "argument")

data ContextType = 
  ContextType {
    contextTypeCtx :: Assertion,
    contextTypeType :: Type}
  deriving (Eq, Ord, Read, Show)

_ContextType = (Core.Name "hydra.ext.haskell.ast.ContextType")

_ContextType_ctx = (Core.Name "ctx")

_ContextType_type = (Core.Name "type")

data FunctionType = 
  FunctionType {
    functionTypeDomain :: Type,
    functionTypeCodomain :: Type}
  deriving (Eq, Ord, Read, Show)

_FunctionType = (Core.Name "hydra.ext.haskell.ast.FunctionType")

_FunctionType_domain = (Core.Name "domain")

_FunctionType_codomain = (Core.Name "codomain")

data InfixType = 
  InfixType {
    infixTypeLhs :: Type,
    infixTypeOperator :: Operator,
    infixTypeRhs :: Operator}
  deriving (Eq, Ord, Read, Show)

_InfixType = (Core.Name "hydra.ext.haskell.ast.InfixType")

_InfixType_lhs = (Core.Name "lhs")

_InfixType_operator = (Core.Name "operator")

_InfixType_rhs = (Core.Name "rhs")

data TypeDeclaration = 
  TypeDeclaration {
    typeDeclarationName :: DeclarationHead,
    typeDeclarationType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeDeclaration = (Core.Name "hydra.ext.haskell.ast.TypeDeclaration")

_TypeDeclaration_name = (Core.Name "name")

_TypeDeclaration_type = (Core.Name "type")

data TypeSignature = 
  TypeSignature {
    typeSignatureName :: Name,
    typeSignatureType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeSignature = (Core.Name "hydra.ext.haskell.ast.TypeSignature")

_TypeSignature_name = (Core.Name "name")

_TypeSignature_type = (Core.Name "type")

data TypedBinding = 
  TypedBinding {
    typedBindingTypeSignature :: TypeSignature,
    typedBindingValueBinding :: ValueBinding}
  deriving (Eq, Ord, Read, Show)

_TypedBinding = (Core.Name "hydra.ext.haskell.ast.TypedBinding")

_TypedBinding_typeSignature = (Core.Name "typeSignature")

_TypedBinding_valueBinding = (Core.Name "valueBinding")

data ValueBinding = 
  ValueBindingSimple SimpleValueBinding
  deriving (Eq, Ord, Read, Show)

_ValueBinding = (Core.Name "hydra.ext.haskell.ast.ValueBinding")

_ValueBinding_simple = (Core.Name "simple")

data SimpleValueBinding = 
  SimpleValueBinding {
    simpleValueBindingPattern :: Pattern,
    simpleValueBindingRhs :: RightHandSide,
    simpleValueBindingLocalBindings :: (Maybe LocalBindings)}
  deriving (Eq, Ord, Read, Show)

_SimpleValueBinding = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding")

_SimpleValueBinding_pattern = (Core.Name "pattern")

_SimpleValueBinding_rhs = (Core.Name "rhs")

_SimpleValueBinding_localBindings = (Core.Name "localBindings")

newtype Variable = 
  Variable {
    unVariable :: Name}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra.ext.haskell.ast.Variable")
