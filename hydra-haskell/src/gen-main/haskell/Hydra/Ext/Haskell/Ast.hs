-- Note: this is an automatically generated file. Do not edit.

-- | A Haskell syntax model, loosely based on Language.Haskell.Tools.AST

module Hydra.Ext.Haskell.Ast where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A pattern-matching alternative
data Alternative = 
  Alternative {
    -- | The pattern to match
    alternativePattern :: Pattern,
    -- | The right-hand side of the alternative
    alternativeRhs :: CaseRhs,
    -- | Optional local bindings
    alternativeBinds :: (Maybe LocalBindings)}
  deriving (Eq, Ord, Read, Show)

_Alternative = (Core.Name "hydra.ext.haskell.ast.Alternative")

_Alternative_pattern = (Core.Name "pattern")

_Alternative_rhs = (Core.Name "rhs")

_Alternative_binds = (Core.Name "binds")

-- | A type assertion
data Assertion = 
  -- | A class assertion
  AssertionClass ClassAssertion |
  -- | A tuple of assertions
  AssertionTuple [Assertion]
  deriving (Eq, Ord, Read, Show)

_Assertion = (Core.Name "hydra.ext.haskell.ast.Assertion")

_Assertion_class = (Core.Name "class")

_Assertion_tuple = (Core.Name "tuple")

-- | A class assertion
data ClassAssertion = 
  ClassAssertion {
    -- | The name of the class
    classAssertionName :: Name,
    -- | The types to which the class is applied
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
  -- | An ordinary (positional) constructor
  ConstructorOrdinary OrdinaryConstructor |
  -- | A record constructor
  ConstructorRecord RecordConstructor
  deriving (Eq, Ord, Read, Show)

_Constructor = (Core.Name "hydra.ext.haskell.ast.Constructor")

_Constructor_ordinary = (Core.Name "ordinary")

_Constructor_record = (Core.Name "record")

-- | An ordinary (positional) data constructor
data OrdinaryConstructor = 
  OrdinaryConstructor {
    -- | The name of the constructor
    ordinaryConstructorName :: Name,
    -- | The types of the positional fields
    ordinaryConstructorFields :: [Type]}
  deriving (Eq, Ord, Read, Show)

_OrdinaryConstructor = (Core.Name "hydra.ext.haskell.ast.OrdinaryConstructor")

_OrdinaryConstructor_name = (Core.Name "name")

_OrdinaryConstructor_fields = (Core.Name "fields")

-- | A record-style data constructor
data RecordConstructor = 
  RecordConstructor {
    -- | The name of the constructor
    recordConstructorName :: Name,
    -- | The named fields of the record
    recordConstructorFields :: [FieldWithComments]}
  deriving (Eq, Ord, Read, Show)

_RecordConstructor = (Core.Name "hydra.ext.haskell.ast.RecordConstructor")

_RecordConstructor_name = (Core.Name "name")

_RecordConstructor_fields = (Core.Name "fields")

-- | A data constructor together with any comments
data ConstructorWithComments = 
  ConstructorWithComments {
    -- | The constructor
    constructorWithCommentsBody :: Constructor,
    -- | Optional comments
    constructorWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_ConstructorWithComments = (Core.Name "hydra.ext.haskell.ast.ConstructorWithComments")

_ConstructorWithComments_body = (Core.Name "body")

_ConstructorWithComments_comments = (Core.Name "comments")

-- | A data type declaration
data DataDeclaration = 
  DataDeclaration {
    -- | The 'data' or 'newtype' keyword
    dataDeclarationKeyword :: DataOrNewtype,
    -- | Type class constraints
    dataDeclarationContext :: [Assertion],
    -- | The declaration head
    dataDeclarationHead :: DeclarationHead,
    -- | The data constructors
    dataDeclarationConstructors :: [ConstructorWithComments],
    -- | Derived type class instances
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
    -- | The declaration
    declarationWithCommentsBody :: Declaration,
    -- | Optional comments
    declarationWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_DeclarationWithComments = (Core.Name "hydra.ext.haskell.ast.DeclarationWithComments")

_DeclarationWithComments_body = (Core.Name "body")

_DeclarationWithComments_comments = (Core.Name "comments")

-- | A data or value declaration
data Declaration = 
  -- | A data type declaration
  DeclarationData DataDeclaration |
  -- | A type synonym declaration
  DeclarationType TypeDeclaration |
  -- | A value binding
  DeclarationValueBinding ValueBinding |
  -- | A typed binding
  DeclarationTypedBinding TypedBinding
  deriving (Eq, Ord, Read, Show)

_Declaration = (Core.Name "hydra.ext.haskell.ast.Declaration")

_Declaration_data = (Core.Name "data")

_Declaration_type = (Core.Name "type")

_Declaration_valueBinding = (Core.Name "valueBinding")

_Declaration_typedBinding = (Core.Name "typedBinding")

-- | The left-hand side of a declaration
data DeclarationHead = 
  -- | An application-style declaration head
  DeclarationHeadApplication ApplicationDeclarationHead |
  -- | A parenthesized declaration head
  DeclarationHeadParens DeclarationHead |
  -- | A simple name
  DeclarationHeadSimple Name
  deriving (Eq, Ord, Read, Show)

_DeclarationHead = (Core.Name "hydra.ext.haskell.ast.DeclarationHead")

_DeclarationHead_application = (Core.Name "application")

_DeclarationHead_parens = (Core.Name "parens")

_DeclarationHead_simple = (Core.Name "simple")

-- | An application-style declaration head
data ApplicationDeclarationHead = 
  ApplicationDeclarationHead {
    -- | The function being applied
    applicationDeclarationHeadFunction :: DeclarationHead,
    -- | The type variable operand
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
  -- | An exported declaration
  ExportDeclaration ImportExportSpec |
  -- | An exported module
  ExportModule ModuleName
  deriving (Eq, Ord, Read, Show)

_Export = (Core.Name "hydra.ext.haskell.ast.Export")

_Export_declaration = (Core.Name "declaration")

_Export_module = (Core.Name "module")

-- | A data expression
data Expression = 
  -- | A function application
  ExpressionApplication ApplicationExpression |
  -- | A case expression
  ExpressionCase CaseExpression |
  -- | A record constructor expression
  ExpressionConstructRecord ConstructRecordExpression |
  -- | A 'do' expression
  ExpressionDo [Statement] |
  -- | An 'if' expression
  ExpressionIf IfExpression |
  -- | An infix application
  ExpressionInfixApplication InfixApplicationExpression |
  -- | A literal value
  ExpressionLiteral Literal |
  -- | A lambda expression
  ExpressionLambda LambdaExpression |
  -- | A left section expression
  ExpressionLeftSection SectionExpression |
  -- | A 'let' expression
  ExpressionLet LetExpression |
  -- | A list expression
  ExpressionList [Expression] |
  -- | A parenthesized expression
  ExpressionParens Expression |
  -- | A prefix application
  ExpressionPrefixApplication PrefixApplicationExpression |
  -- | A right section expression
  ExpressionRightSection SectionExpression |
  -- | A tuple expression
  ExpressionTuple [Expression] |
  -- | A type signature expression
  ExpressionTypeSignature TypeSignatureExpression |
  -- | A record update expression
  ExpressionUpdateRecord UpdateRecordExpression |
  -- | A variable reference
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
    -- | The function being applied
    applicationExpressionFunction :: Expression,
    -- | The argument
    applicationExpressionArgument :: Expression}
  deriving (Eq, Ord, Read, Show)

_ApplicationExpression = (Core.Name "hydra.ext.haskell.ast.ApplicationExpression")

_ApplicationExpression_function = (Core.Name "function")

_ApplicationExpression_argument = (Core.Name "argument")

-- | A case expression
data CaseExpression = 
  CaseExpression {
    -- | The expression being matched
    caseExpressionCase :: Expression,
    -- | The pattern-matching alternatives
    caseExpressionAlternatives :: [Alternative]}
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra.ext.haskell.ast.CaseExpression")

_CaseExpression_case = (Core.Name "case")

_CaseExpression_alternatives = (Core.Name "alternatives")

-- | A record constructor expression
data ConstructRecordExpression = 
  ConstructRecordExpression {
    -- | The constructor name
    constructRecordExpressionName :: Name,
    -- | The field assignments
    constructRecordExpressionFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)

_ConstructRecordExpression = (Core.Name "hydra.ext.haskell.ast.ConstructRecordExpression")

_ConstructRecordExpression_name = (Core.Name "name")

_ConstructRecordExpression_fields = (Core.Name "fields")

-- | An 'if' expression
data IfExpression = 
  IfExpression {
    -- | The condition expression
    ifExpressionCondition :: Expression,
    -- | The 'then' branch
    ifExpressionThen :: Expression,
    -- | The 'else' branch
    ifExpressionElse :: Expression}
  deriving (Eq, Ord, Read, Show)

_IfExpression = (Core.Name "hydra.ext.haskell.ast.IfExpression")

_IfExpression_condition = (Core.Name "condition")

_IfExpression_then = (Core.Name "then")

_IfExpression_else = (Core.Name "else")

-- | An infix application expression
data InfixApplicationExpression = 
  InfixApplicationExpression {
    -- | The left-hand operand
    infixApplicationExpressionLhs :: Expression,
    -- | The infix operator
    infixApplicationExpressionOperator :: Operator,
    -- | The right-hand operand
    infixApplicationExpressionRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_InfixApplicationExpression = (Core.Name "hydra.ext.haskell.ast.InfixApplicationExpression")

_InfixApplicationExpression_lhs = (Core.Name "lhs")

_InfixApplicationExpression_operator = (Core.Name "operator")

_InfixApplicationExpression_rhs = (Core.Name "rhs")

-- | A lambda expression
data LambdaExpression = 
  LambdaExpression {
    -- | The patterns binding parameters
    lambdaExpressionBindings :: [Pattern],
    -- | The body of the lambda
    lambdaExpressionInner :: Expression}
  deriving (Eq, Ord, Read, Show)

_LambdaExpression = (Core.Name "hydra.ext.haskell.ast.LambdaExpression")

_LambdaExpression_bindings = (Core.Name "bindings")

_LambdaExpression_inner = (Core.Name "inner")

-- | A 'let' expression
data LetExpression = 
  LetExpression {
    -- | The local bindings
    letExpressionBindings :: [LocalBinding],
    -- | The body of the let expression
    letExpressionInner :: Expression}
  deriving (Eq, Ord, Read, Show)

_LetExpression = (Core.Name "hydra.ext.haskell.ast.LetExpression")

_LetExpression_bindings = (Core.Name "bindings")

_LetExpression_inner = (Core.Name "inner")

-- | A prefix expression
data PrefixApplicationExpression = 
  PrefixApplicationExpression {
    -- | The prefix operator
    prefixApplicationExpressionOperator :: Operator,
    -- | The operand
    prefixApplicationExpressionRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_PrefixApplicationExpression = (Core.Name "hydra.ext.haskell.ast.PrefixApplicationExpression")

_PrefixApplicationExpression_operator = (Core.Name "operator")

_PrefixApplicationExpression_rhs = (Core.Name "rhs")

-- | A section expression
data SectionExpression = 
  SectionExpression {
    -- | The operator
    sectionExpressionOperator :: Operator,
    -- | The operand
    sectionExpressionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_SectionExpression = (Core.Name "hydra.ext.haskell.ast.SectionExpression")

_SectionExpression_operator = (Core.Name "operator")

_SectionExpression_expression = (Core.Name "expression")

-- | A type signature expression
data TypeSignatureExpression = 
  TypeSignatureExpression {
    -- | The expression being typed
    typeSignatureExpressionInner :: Expression,
    -- | The type signature
    typeSignatureExpressionType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeSignatureExpression = (Core.Name "hydra.ext.haskell.ast.TypeSignatureExpression")

_TypeSignatureExpression_inner = (Core.Name "inner")

_TypeSignatureExpression_type = (Core.Name "type")

-- | An update record expression
data UpdateRecordExpression = 
  UpdateRecordExpression {
    -- | The record being updated
    updateRecordExpressionInner :: Expression,
    -- | The field updates
    updateRecordExpressionFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)

_UpdateRecordExpression = (Core.Name "hydra.ext.haskell.ast.UpdateRecordExpression")

_UpdateRecordExpression_inner = (Core.Name "inner")

_UpdateRecordExpression_fields = (Core.Name "fields")

-- | A field (name/type pair)
data Field = 
  Field {
    -- | The field name
    fieldName :: Name,
    -- | The field type
    fieldType :: Type}
  deriving (Eq, Ord, Read, Show)

_Field = (Core.Name "hydra.ext.haskell.ast.Field")

_Field_name = (Core.Name "name")

_Field_type = (Core.Name "type")

-- | A field together with any comments
data FieldWithComments = 
  FieldWithComments {
    -- | The field
    fieldWithCommentsField :: Field,
    -- | Optional comments
    fieldWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_FieldWithComments = (Core.Name "hydra.ext.haskell.ast.FieldWithComments")

_FieldWithComments_field = (Core.Name "field")

_FieldWithComments_comments = (Core.Name "comments")

-- | A field name and value
data FieldUpdate = 
  FieldUpdate {
    -- | The field name
    fieldUpdateName :: Name,
    -- | The field value
    fieldUpdateValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_FieldUpdate = (Core.Name "hydra.ext.haskell.ast.FieldUpdate")

_FieldUpdate_name = (Core.Name "name")

_FieldUpdate_value = (Core.Name "value")

-- | An import statement
data Import = 
  Import {
    -- | Whether the import is qualified
    importQualified :: Bool,
    -- | The module being imported
    importModule :: ModuleName,
    -- | Optional alias for the module
    importAs :: (Maybe ModuleName),
    -- | Optional import specification
    importSpec :: (Maybe SpecImport)}
  deriving (Eq, Ord, Read, Show)

_Import = (Core.Name "hydra.ext.haskell.ast.Import")

_Import_qualified = (Core.Name "qualified")

_Import_module = (Core.Name "module")

_Import_as = (Core.Name "as")

_Import_spec = (Core.Name "spec")

-- | An import specification
data SpecImport = 
  -- | A list of imports to include
  SpecImportList [ImportExportSpec] |
  -- | A list of imports to exclude
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
    -- | Optional import modifier
    importExportSpecModifier :: (Maybe ImportModifier),
    -- | The name being imported or exported
    importExportSpecName :: Name,
    -- | Optional subspecification
    importExportSpecSubspec :: (Maybe SubspecImportExportSpec)}
  deriving (Eq, Ord, Read, Show)

_ImportExportSpec = (Core.Name "hydra.ext.haskell.ast.ImportExportSpec")

_ImportExportSpec_modifier = (Core.Name "modifier")

_ImportExportSpec_name = (Core.Name "name")

_ImportExportSpec_subspec = (Core.Name "subspec")

-- | A subspecification within an import/export
data SubspecImportExportSpec = 
  -- | Import/export all
  SubspecImportExportSpecAll  |
  -- | Import/export specific names
  SubspecImportExportSpecList [Name]
  deriving (Eq, Ord, Read, Show)

_SubspecImportExportSpec = (Core.Name "hydra.ext.haskell.ast.SubspecImportExportSpec")

_SubspecImportExportSpec_all = (Core.Name "all")

_SubspecImportExportSpec_list = (Core.Name "list")

-- | A literal value
data Literal = 
  -- | A character literal
  LiteralChar Int |
  -- | A double-precision floating point literal
  LiteralDouble Double |
  -- | A single-precision floating point literal
  LiteralFloat Float |
  -- | A 32-bit integer literal
  LiteralInt Int |
  -- | An arbitrary-precision integer literal
  LiteralInteger Integer |
  -- | A string literal
  LiteralString String
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.haskell.ast.Literal")

_Literal_char = (Core.Name "char")

_Literal_double = (Core.Name "double")

_Literal_float = (Core.Name "float")

_Literal_int = (Core.Name "int")

_Literal_integer = (Core.Name "integer")

_Literal_string = (Core.Name "string")

-- | A local binding
data LocalBinding = 
  -- | A type signature
  LocalBindingSignature TypeSignature |
  -- | A value binding
  LocalBindingValue ValueBinding
  deriving (Eq, Ord, Read, Show)

_LocalBinding = (Core.Name "hydra.ext.haskell.ast.LocalBinding")

_LocalBinding_signature = (Core.Name "signature")

_LocalBinding_value = (Core.Name "value")

-- | A collection of local bindings
newtype LocalBindings = 
  LocalBindings {
    unLocalBindings :: [LocalBinding]}
  deriving (Eq, Ord, Read, Show)

_LocalBindings = (Core.Name "hydra.ext.haskell.ast.LocalBindings")

-- | A Haskell module
data Module = 
  Module {
    -- | Optional module head
    moduleHead :: (Maybe ModuleHead),
    -- | Import statements
    moduleImports :: [Import],
    -- | Module declarations
    moduleDeclarations :: [DeclarationWithComments]}
  deriving (Eq, Ord, Read, Show)

_Module = (Core.Name "hydra.ext.haskell.ast.Module")

_Module_head = (Core.Name "head")

_Module_imports = (Core.Name "imports")

_Module_declarations = (Core.Name "declarations")

-- | A module head
data ModuleHead = 
  ModuleHead {
    -- | Optional module-level comments
    moduleHeadComments :: (Maybe String),
    -- | The module name
    moduleHeadName :: ModuleName,
    -- | Export list
    moduleHeadExports :: [Export]}
  deriving (Eq, Ord, Read, Show)

_ModuleHead = (Core.Name "hydra.ext.haskell.ast.ModuleHead")

_ModuleHead_comments = (Core.Name "comments")

_ModuleHead_name = (Core.Name "name")

_ModuleHead_exports = (Core.Name "exports")

-- | A module name
newtype ModuleName = 
  ModuleName {
    unModuleName :: String}
  deriving (Eq, Ord, Read, Show)

_ModuleName = (Core.Name "hydra.ext.haskell.ast.ModuleName")

-- | A name
data Name = 
  -- | An implicit name
  NameImplicit QualifiedName |
  -- | A normal name
  NameNormal QualifiedName |
  -- | A parenthesized name
  NameParens QualifiedName
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra.ext.haskell.ast.Name")

_Name_implicit = (Core.Name "implicit")

_Name_normal = (Core.Name "normal")

_Name_parens = (Core.Name "parens")

-- | A component of a qualified name
newtype NamePart = 
  NamePart {
    unNamePart :: String}
  deriving (Eq, Ord, Read, Show)

_NamePart = (Core.Name "hydra.ext.haskell.ast.NamePart")

-- | An operator
data Operator = 
  -- | A function used as an infix operator
  OperatorBacktick QualifiedName |
  -- | A normal infix operator
  OperatorNormal QualifiedName
  deriving (Eq, Ord, Read, Show)

_Operator = (Core.Name "hydra.ext.haskell.ast.Operator")

_Operator_backtick = (Core.Name "backtick")

_Operator_normal = (Core.Name "normal")

-- | A pattern
data Pattern = 
  -- | An application pattern
  PatternApplication ApplicationPattern |
  -- | An 'as' pattern
  PatternAs AsPattern |
  -- | A list pattern
  PatternList [Pattern] |
  -- | A literal pattern
  PatternLiteral Literal |
  -- | A name pattern
  PatternName Name |
  -- | A parenthesized pattern
  PatternParens Pattern |
  -- | A record pattern
  PatternRecord RecordPattern |
  -- | A tuple pattern
  PatternTuple [Pattern] |
  -- | A typed pattern
  PatternTyped TypedPattern |
  -- | A wildcard pattern
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

-- | An application pattern
data ApplicationPattern = 
  ApplicationPattern {
    -- | The constructor name
    applicationPatternName :: Name,
    -- | The pattern arguments
    applicationPatternArgs :: [Pattern]}
  deriving (Eq, Ord, Read, Show)

_ApplicationPattern = (Core.Name "hydra.ext.haskell.ast.ApplicationPattern")

_ApplicationPattern_name = (Core.Name "name")

_ApplicationPattern_args = (Core.Name "args")

-- | An 'as' pattern
data AsPattern = 
  AsPattern {
    -- | The bound name
    asPatternName :: Name,
    -- | The inner pattern
    asPatternInner :: Pattern}
  deriving (Eq, Ord, Read, Show)

_AsPattern = (Core.Name "hydra.ext.haskell.ast.AsPattern")

_AsPattern_name = (Core.Name "name")

_AsPattern_inner = (Core.Name "inner")

-- | A record pattern
data RecordPattern = 
  RecordPattern {
    -- | The constructor name
    recordPatternName :: Name,
    -- | The field patterns
    recordPatternFields :: [PatternField]}
  deriving (Eq, Ord, Read, Show)

_RecordPattern = (Core.Name "hydra.ext.haskell.ast.RecordPattern")

_RecordPattern_name = (Core.Name "name")

_RecordPattern_fields = (Core.Name "fields")

-- | A typed pattern
data TypedPattern = 
  TypedPattern {
    -- | The inner pattern
    typedPatternInner :: Pattern,
    -- | The type annotation
    typedPatternType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypedPattern = (Core.Name "hydra.ext.haskell.ast.TypedPattern")

_TypedPattern_inner = (Core.Name "inner")

_TypedPattern_type = (Core.Name "type")

-- | A pattern field
data PatternField = 
  PatternField {
    -- | The field name
    patternFieldName :: Name,
    -- | The field pattern
    patternFieldPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_PatternField = (Core.Name "hydra.ext.haskell.ast.PatternField")

_PatternField_name = (Core.Name "name")

_PatternField_pattern = (Core.Name "pattern")

-- | A qualified name
data QualifiedName = 
  QualifiedName {
    -- | The qualifier parts
    qualifiedNameQualifiers :: [NamePart],
    -- | The unqualified name part
    qualifiedNameUnqualified :: NamePart}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra.ext.haskell.ast.QualifiedName")

_QualifiedName_qualifiers = (Core.Name "qualifiers")

_QualifiedName_unqualified = (Core.Name "unqualified")

-- | A right-hand side of a binding
newtype RightHandSide = 
  RightHandSide {
    unRightHandSide :: Expression}
  deriving (Eq, Ord, Read, Show)

_RightHandSide = (Core.Name "hydra.ext.haskell.ast.RightHandSide")

-- | A do-notation statement
newtype Statement = 
  Statement {
    unStatement :: Expression}
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra.ext.haskell.ast.Statement")

-- | A type expression
data Type = 
  -- | An application type
  TypeApplication ApplicationType |
  -- | A context type
  TypeCtx ContextType |
  -- | A function type
  TypeFunction FunctionType |
  -- | An infix type
  TypeInfix InfixType |
  -- | A list type
  TypeList Type |
  -- | A parenthesized type
  TypeParens Type |
  -- | A tuple type
  TypeTuple [Type] |
  -- | A type variable or type name
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

-- | An application type
data ApplicationType = 
  ApplicationType {
    -- | The type being applied
    applicationTypeContext :: Type,
    -- | The type argument
    applicationTypeArgument :: Type}
  deriving (Eq, Ord, Read, Show)

_ApplicationType = (Core.Name "hydra.ext.haskell.ast.ApplicationType")

_ApplicationType_context = (Core.Name "context")

_ApplicationType_argument = (Core.Name "argument")

-- | A type with a context (type class constraints)
data ContextType = 
  ContextType {
    -- | The type class context
    contextTypeCtx :: Assertion,
    -- | The constrained type
    contextTypeType :: Type}
  deriving (Eq, Ord, Read, Show)

_ContextType = (Core.Name "hydra.ext.haskell.ast.ContextType")

_ContextType_ctx = (Core.Name "ctx")

_ContextType_type = (Core.Name "type")

-- | A function type
data FunctionType = 
  FunctionType {
    -- | The domain type
    functionTypeDomain :: Type,
    -- | The codomain type
    functionTypeCodomain :: Type}
  deriving (Eq, Ord, Read, Show)

_FunctionType = (Core.Name "hydra.ext.haskell.ast.FunctionType")

_FunctionType_domain = (Core.Name "domain")

_FunctionType_codomain = (Core.Name "codomain")

-- | An infix type application
data InfixType = 
  InfixType {
    -- | The left-hand type
    infixTypeLhs :: Type,
    -- | The type operator
    infixTypeOperator :: Operator,
    -- | The right-hand operator
    infixTypeRhs :: Operator}
  deriving (Eq, Ord, Read, Show)

_InfixType = (Core.Name "hydra.ext.haskell.ast.InfixType")

_InfixType_lhs = (Core.Name "lhs")

_InfixType_operator = (Core.Name "operator")

_InfixType_rhs = (Core.Name "rhs")

-- | A type synonym declaration
data TypeDeclaration = 
  TypeDeclaration {
    -- | The declaration head
    typeDeclarationName :: DeclarationHead,
    -- | The type being defined
    typeDeclarationType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeDeclaration = (Core.Name "hydra.ext.haskell.ast.TypeDeclaration")

_TypeDeclaration_name = (Core.Name "name")

_TypeDeclaration_type = (Core.Name "type")

-- | A type signature
data TypeSignature = 
  TypeSignature {
    -- | The name being typed
    typeSignatureName :: Name,
    -- | The type
    typeSignatureType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeSignature = (Core.Name "hydra.ext.haskell.ast.TypeSignature")

_TypeSignature_name = (Core.Name "name")

_TypeSignature_type = (Core.Name "type")

-- | A binding with its type signature
data TypedBinding = 
  TypedBinding {
    -- | The type signature
    typedBindingTypeSignature :: TypeSignature,
    -- | The value binding
    typedBindingValueBinding :: ValueBinding}
  deriving (Eq, Ord, Read, Show)

_TypedBinding = (Core.Name "hydra.ext.haskell.ast.TypedBinding")

_TypedBinding_typeSignature = (Core.Name "typeSignature")

_TypedBinding_valueBinding = (Core.Name "valueBinding")

-- | A value binding
data ValueBinding = 
  -- | A simple value binding
  ValueBindingSimple SimpleValueBinding
  deriving (Eq, Ord, Read, Show)

_ValueBinding = (Core.Name "hydra.ext.haskell.ast.ValueBinding")

_ValueBinding_simple = (Core.Name "simple")

-- | A simple value binding
data SimpleValueBinding = 
  SimpleValueBinding {
    -- | The pattern being bound
    simpleValueBindingPattern :: Pattern,
    -- | The right-hand side
    simpleValueBindingRhs :: RightHandSide,
    -- | Optional local bindings (where clause)
    simpleValueBindingLocalBindings :: (Maybe LocalBindings)}
  deriving (Eq, Ord, Read, Show)

_SimpleValueBinding = (Core.Name "hydra.ext.haskell.ast.SimpleValueBinding")

_SimpleValueBinding_pattern = (Core.Name "pattern")

_SimpleValueBinding_rhs = (Core.Name "rhs")

_SimpleValueBinding_localBindings = (Core.Name "localBindings")

-- | A type variable
newtype Variable = 
  Variable {
    unVariable :: Name}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra.ext.haskell.ast.Variable")
