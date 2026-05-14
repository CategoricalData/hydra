-- Note: this is an automatically generated file. Do not edit.
-- | A Haskell syntax model for Hydra. Originally inspired by Language.Haskell.Tools.AST, but now diverges freely to suit Hydra's needs.

module Hydra.Haskell.Syntax where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
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
_Alternative = Core.Name "hydra.haskell.syntax.Alternative"
_Alternative_pattern = Core.Name "pattern"
_Alternative_rhs = Core.Name "rhs"
_Alternative_binds = Core.Name "binds"
-- | A type constraint
data Constraint =
  -- | A class constraint
  ConstraintClass ClassConstraint |
  -- | A tuple of constraints
  ConstraintTuple [Constraint]
  deriving (Eq, Ord, Read, Show)
_Constraint = Core.Name "hydra.haskell.syntax.Constraint"
_Constraint_class = Core.Name "class"
_Constraint_tuple = Core.Name "tuple"
-- | A class constraint
data ClassConstraint =
  ClassConstraint {
    -- | The name of the class
    classConstraintName :: Name,
    -- | The types to which the class is applied
    classConstraintTypes :: [Type]}
  deriving (Eq, Ord, Read, Show)
_ClassConstraint = Core.Name "hydra.haskell.syntax.ClassConstraint"
_ClassConstraint_name = Core.Name "name"
_ClassConstraint_types = Core.Name "types"
-- | The right-hand side of a pattern-matching alternative
newtype CaseRhs =
  CaseRhs {
    unCaseRhs :: Expression}
  deriving (Eq, Ord, Read, Show)
_CaseRhs = Core.Name "hydra.haskell.syntax.CaseRhs"
-- | A data constructor
data Constructor =
  -- | An ordinary (positional) constructor
  ConstructorOrdinary PositionalConstructor |
  -- | A record constructor
  ConstructorRecord RecordConstructor
  deriving (Eq, Ord, Read, Show)
_Constructor = Core.Name "hydra.haskell.syntax.Constructor"
_Constructor_ordinary = Core.Name "ordinary"
_Constructor_record = Core.Name "record"
-- | An ordinary (positional) data constructor
data PositionalConstructor =
  PositionalConstructor {
    -- | The name of the constructor
    positionalConstructorName :: Name,
    -- | The types of the positional fields
    positionalConstructorFields :: [Type],
    -- | Optional comments
    positionalConstructorComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_PositionalConstructor = Core.Name "hydra.haskell.syntax.PositionalConstructor"
_PositionalConstructor_name = Core.Name "name"
_PositionalConstructor_fields = Core.Name "fields"
_PositionalConstructor_comments = Core.Name "comments"
-- | A record-style data constructor
data RecordConstructor =
  RecordConstructor {
    -- | The name of the constructor
    recordConstructorName :: Name,
    -- | The named fields of the record
    recordConstructorFields :: [Field],
    -- | Optional comments
    recordConstructorComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_RecordConstructor = Core.Name "hydra.haskell.syntax.RecordConstructor"
_RecordConstructor_name = Core.Name "name"
_RecordConstructor_fields = Core.Name "fields"
_RecordConstructor_comments = Core.Name "comments"
-- | A data type declaration
data DataDeclaration =
  DataDeclaration {
    -- | The 'data' or 'newtype' keyword
    dataDeclarationKeyword :: DataKeyword,
    -- | Type class constraints
    dataDeclarationContext :: [Constraint],
    -- | The declaration head
    dataDeclarationHead :: DeclarationHead,
    -- | The data constructors
    dataDeclarationConstructors :: [Constructor],
    -- | Derived type class instances
    dataDeclarationDeriving :: [DerivingClause],
    -- | Optional comments
    dataDeclarationComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_DataDeclaration = Core.Name "hydra.haskell.syntax.DataDeclaration"
_DataDeclaration_keyword = Core.Name "keyword"
_DataDeclaration_context = Core.Name "context"
_DataDeclaration_head = Core.Name "head"
_DataDeclaration_constructors = Core.Name "constructors"
_DataDeclaration_deriving = Core.Name "deriving"
_DataDeclaration_comments = Core.Name "comments"
-- | The 'data' versus 'newtype keyword
data DataKeyword =
  DataKeywordData |
  DataKeywordNewtype
  deriving (Eq, Ord, Read, Show)
_DataKeyword = Core.Name "hydra.haskell.syntax.DataKeyword"
_DataKeyword_data = Core.Name "data"
_DataKeyword_newtype = Core.Name "newtype"
-- | A data or value declaration
data Declaration =
  -- | A data type declaration
  DeclarationData DataDeclaration |
  -- | A type synonym declaration
  DeclarationType TypeSynonymDeclaration |
  -- | A value binding
  DeclarationValueBinding ValueBinding |
  -- | A typed binding
  DeclarationTypedBinding TypedBinding
  deriving (Eq, Ord, Read, Show)
_Declaration = Core.Name "hydra.haskell.syntax.Declaration"
_Declaration_data = Core.Name "data"
_Declaration_type = Core.Name "type"
_Declaration_valueBinding = Core.Name "valueBinding"
_Declaration_typedBinding = Core.Name "typedBinding"
-- | The left-hand side of a declaration
data DeclarationHead =
  -- | An application-style declaration head
  DeclarationHeadApplication ApplicationDeclarationHead |
  -- | A simple name
  DeclarationHeadSimple Name
  deriving (Eq, Ord, Read, Show)
_DeclarationHead = Core.Name "hydra.haskell.syntax.DeclarationHead"
_DeclarationHead_application = Core.Name "application"
_DeclarationHead_simple = Core.Name "simple"
-- | An application-style declaration head
data ApplicationDeclarationHead =
  ApplicationDeclarationHead {
    -- | The function being applied
    applicationDeclarationHeadFunction :: DeclarationHead,
    -- | The type variable operand
    applicationDeclarationHeadOperand :: Variable}
  deriving (Eq, Ord, Read, Show)
_ApplicationDeclarationHead = Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"
_ApplicationDeclarationHead_function = Core.Name "function"
_ApplicationDeclarationHead_operand = Core.Name "operand"
-- | A 'deriving' clause
newtype DerivingClause =
  DerivingClause {
    unDerivingClause :: [Name]}
  deriving (Eq, Ord, Read, Show)
_DerivingClause = Core.Name "hydra.haskell.syntax.DerivingClause"
-- | An export statement
data Export =
  -- | An exported declaration
  ExportDeclaration NamedImportExport |
  -- | An exported module
  ExportModule ModuleName
  deriving (Eq, Ord, Read, Show)
_Export = Core.Name "hydra.haskell.syntax.Export"
_Export_declaration = Core.Name "declaration"
_Export_module = Core.Name "module"
-- | A data expression
data Expression =
  -- | A function application
  ExpressionApplication ApplicationExpression |
  -- | A case expression
  ExpressionCase CaseExpression |
  -- | A record constructor expression
  ExpressionConstructRecord RecordExpression |
  -- | A 'do' expression
  ExpressionDo [Statement] |
  -- | An 'if' expression
  ExpressionIf IfExpression |
  -- | An infix application
  ExpressionInfixApplication InfixExpression |
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
  -- | A right section expression
  ExpressionRightSection SectionExpression |
  -- | A tuple expression
  ExpressionTuple [Expression] |
  -- | A type signature expression
  ExpressionTypeSignature TypedExpression |
  -- | A record update expression
  ExpressionUpdateRecord RecordUpdateExpression |
  -- | A variable reference
  ExpressionVariable Name
  deriving (Eq, Ord, Read, Show)
_Expression = Core.Name "hydra.haskell.syntax.Expression"
_Expression_application = Core.Name "application"
_Expression_case = Core.Name "case"
_Expression_constructRecord = Core.Name "constructRecord"
_Expression_do = Core.Name "do"
_Expression_if = Core.Name "if"
_Expression_infixApplication = Core.Name "infixApplication"
_Expression_literal = Core.Name "literal"
_Expression_lambda = Core.Name "lambda"
_Expression_leftSection = Core.Name "leftSection"
_Expression_let = Core.Name "let"
_Expression_list = Core.Name "list"
_Expression_rightSection = Core.Name "rightSection"
_Expression_tuple = Core.Name "tuple"
_Expression_typeSignature = Core.Name "typeSignature"
_Expression_updateRecord = Core.Name "updateRecord"
_Expression_variable = Core.Name "variable"
-- | An application expression
data ApplicationExpression =
  ApplicationExpression {
    -- | The function being applied
    applicationExpressionFunction :: Expression,
    -- | The argument
    applicationExpressionArgument :: Expression}
  deriving (Eq, Ord, Read, Show)
_ApplicationExpression = Core.Name "hydra.haskell.syntax.ApplicationExpression"
_ApplicationExpression_function = Core.Name "function"
_ApplicationExpression_argument = Core.Name "argument"
-- | A case expression
data CaseExpression =
  CaseExpression {
    -- | The expression being matched
    caseExpressionCase :: Expression,
    -- | The pattern-matching alternatives
    caseExpressionAlternatives :: [Alternative]}
  deriving (Eq, Ord, Read, Show)
_CaseExpression = Core.Name "hydra.haskell.syntax.CaseExpression"
_CaseExpression_case = Core.Name "case"
_CaseExpression_alternatives = Core.Name "alternatives"
-- | A record constructor expression
data RecordExpression =
  RecordExpression {
    -- | The constructor name
    recordExpressionName :: Name,
    -- | The field assignments
    recordExpressionFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)
_RecordExpression = Core.Name "hydra.haskell.syntax.RecordExpression"
_RecordExpression_name = Core.Name "name"
_RecordExpression_fields = Core.Name "fields"
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
_IfExpression = Core.Name "hydra.haskell.syntax.IfExpression"
_IfExpression_condition = Core.Name "condition"
_IfExpression_then = Core.Name "then"
_IfExpression_else = Core.Name "else"
-- | An infix application expression
data InfixExpression =
  InfixExpression {
    -- | The left-hand operand
    infixExpressionLhs :: Expression,
    -- | The infix operator
    infixExpressionOperator :: Operator,
    -- | The right-hand operand
    infixExpressionRhs :: Expression}
  deriving (Eq, Ord, Read, Show)
_InfixExpression = Core.Name "hydra.haskell.syntax.InfixExpression"
_InfixExpression_lhs = Core.Name "lhs"
_InfixExpression_operator = Core.Name "operator"
_InfixExpression_rhs = Core.Name "rhs"
-- | A lambda expression
data LambdaExpression =
  LambdaExpression {
    -- | The patterns binding parameters
    lambdaExpressionBindings :: [Pattern],
    -- | The body of the lambda
    lambdaExpressionInner :: Expression}
  deriving (Eq, Ord, Read, Show)
_LambdaExpression = Core.Name "hydra.haskell.syntax.LambdaExpression"
_LambdaExpression_bindings = Core.Name "bindings"
_LambdaExpression_inner = Core.Name "inner"
-- | A 'let' expression
data LetExpression =
  LetExpression {
    -- | The local bindings
    letExpressionBindings :: [LocalBinding],
    -- | The body of the let expression
    letExpressionInner :: Expression}
  deriving (Eq, Ord, Read, Show)
_LetExpression = Core.Name "hydra.haskell.syntax.LetExpression"
_LetExpression_bindings = Core.Name "bindings"
_LetExpression_inner = Core.Name "inner"
-- | A section expression
data SectionExpression =
  SectionExpression {
    -- | The operator
    sectionExpressionOperator :: Operator,
    -- | The operand
    sectionExpressionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)
_SectionExpression = Core.Name "hydra.haskell.syntax.SectionExpression"
_SectionExpression_operator = Core.Name "operator"
_SectionExpression_expression = Core.Name "expression"
-- | A type signature expression
data TypedExpression =
  TypedExpression {
    -- | The expression being typed
    typedExpressionInner :: Expression,
    -- | The type signature
    typedExpressionType :: Type}
  deriving (Eq, Ord, Read, Show)
_TypedExpression = Core.Name "hydra.haskell.syntax.TypedExpression"
_TypedExpression_inner = Core.Name "inner"
_TypedExpression_type = Core.Name "type"
-- | An update record expression
data RecordUpdateExpression =
  RecordUpdateExpression {
    -- | The record being updated
    recordUpdateExpressionInner :: Expression,
    -- | The field updates
    recordUpdateExpressionFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)
_RecordUpdateExpression = Core.Name "hydra.haskell.syntax.RecordUpdateExpression"
_RecordUpdateExpression_inner = Core.Name "inner"
_RecordUpdateExpression_fields = Core.Name "fields"
-- | A field (name/type pair)
data Field =
  Field {
    -- | The field name
    fieldName :: Name,
    -- | The field type
    fieldType :: Type,
    -- | Optional comments
    fieldComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_Field = Core.Name "hydra.haskell.syntax.Field"
_Field_name = Core.Name "name"
_Field_type = Core.Name "type"
_Field_comments = Core.Name "comments"
-- | A field name and value
data FieldUpdate =
  FieldUpdate {
    -- | The field name
    fieldUpdateName :: Name,
    -- | The field value
    fieldUpdateValue :: Expression}
  deriving (Eq, Ord, Read, Show)
_FieldUpdate = Core.Name "hydra.haskell.syntax.FieldUpdate"
_FieldUpdate_name = Core.Name "name"
_FieldUpdate_value = Core.Name "value"
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
    importSpec :: (Maybe ImportSpec)}
  deriving (Eq, Ord, Read, Show)
_Import = Core.Name "hydra.haskell.syntax.Import"
_Import_qualified = Core.Name "qualified"
_Import_module = Core.Name "module"
_Import_as = Core.Name "as"
_Import_spec = Core.Name "spec"
-- | An import specification
data ImportSpec =
  -- | A list of imports to include
  ImportSpecList [NamedImportExport] |
  -- | A list of imports to exclude
  ImportSpecHiding [NamedImportExport]
  deriving (Eq, Ord, Read, Show)
_ImportSpec = Core.Name "hydra.haskell.syntax.ImportSpec"
_ImportSpec_list = Core.Name "list"
_ImportSpec_hiding = Core.Name "hiding"
-- | An import modifier ('pattern' or 'type')
data ImportModifier =
  ImportModifierPattern |
  ImportModifierType
  deriving (Eq, Ord, Read, Show)
_ImportModifier = Core.Name "hydra.haskell.syntax.ImportModifier"
_ImportModifier_pattern = Core.Name "pattern"
_ImportModifier_type = Core.Name "type"
-- | An import or export specification
data NamedImportExport =
  NamedImportExport {
    -- | Optional import modifier
    namedImportExportModifier :: (Maybe ImportModifier),
    -- | The name being imported or exported
    namedImportExportName :: Name,
    -- | Optional subspecification
    namedImportExportSubspec :: (Maybe ImportExportSubspec)}
  deriving (Eq, Ord, Read, Show)
_NamedImportExport = Core.Name "hydra.haskell.syntax.NamedImportExport"
_NamedImportExport_modifier = Core.Name "modifier"
_NamedImportExport_name = Core.Name "name"
_NamedImportExport_subspec = Core.Name "subspec"
-- | A subspecification within an import/export
data ImportExportSubspec =
  -- | Import/export all
  ImportExportSubspecAll |
  -- | Import/export specific names
  ImportExportSubspecList [Name]
  deriving (Eq, Ord, Read, Show)
_ImportExportSubspec = Core.Name "hydra.haskell.syntax.ImportExportSubspec"
_ImportExportSubspec_all = Core.Name "all"
_ImportExportSubspec_list = Core.Name "list"
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
_Literal = Core.Name "hydra.haskell.syntax.Literal"
_Literal_char = Core.Name "char"
_Literal_double = Core.Name "double"
_Literal_float = Core.Name "float"
_Literal_int = Core.Name "int"
_Literal_integer = Core.Name "integer"
_Literal_string = Core.Name "string"
-- | A local binding
data LocalBinding =
  -- | A type signature
  LocalBindingSignature TypeSignature |
  -- | A value binding
  LocalBindingValue ValueBinding
  deriving (Eq, Ord, Read, Show)
_LocalBinding = Core.Name "hydra.haskell.syntax.LocalBinding"
_LocalBinding_signature = Core.Name "signature"
_LocalBinding_value = Core.Name "value"
-- | A collection of local bindings
newtype LocalBindings =
  LocalBindings {
    unLocalBindings :: [LocalBinding]}
  deriving (Eq, Ord, Read, Show)
_LocalBindings = Core.Name "hydra.haskell.syntax.LocalBindings"
-- | A Haskell module
data Module =
  Module {
    -- | Optional module head
    moduleHead :: (Maybe ModuleHead),
    -- | Import statements
    moduleImports :: [Import],
    -- | Module declarations
    moduleDeclarations :: [Declaration]}
  deriving (Eq, Ord, Read, Show)
_Module = Core.Name "hydra.haskell.syntax.Module"
_Module_head = Core.Name "head"
_Module_imports = Core.Name "imports"
_Module_declarations = Core.Name "declarations"
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
_ModuleHead = Core.Name "hydra.haskell.syntax.ModuleHead"
_ModuleHead_comments = Core.Name "comments"
_ModuleHead_name = Core.Name "name"
_ModuleHead_exports = Core.Name "exports"
-- | A module name
newtype ModuleName =
  ModuleName {
    unModuleName :: String}
  deriving (Eq, Ord, Read, Show)
_ModuleName = Core.Name "hydra.haskell.syntax.ModuleName"
-- | A name
data Name =
  -- | An implicit name
  NameImplicit QualifiedName |
  -- | A normal name
  NameNormal QualifiedName
  deriving (Eq, Ord, Read, Show)
_Name = Core.Name "hydra.haskell.syntax.Name"
_Name_implicit = Core.Name "implicit"
_Name_normal = Core.Name "normal"
-- | A component of a qualified name
newtype NamePart =
  NamePart {
    unNamePart :: String}
  deriving (Eq, Ord, Read, Show)
_NamePart = Core.Name "hydra.haskell.syntax.NamePart"
-- | An operator
data Operator =
  -- | A function used as an infix operator
  OperatorBacktick QualifiedName |
  -- | A normal infix operator
  OperatorNormal QualifiedName
  deriving (Eq, Ord, Read, Show)
_Operator = Core.Name "hydra.haskell.syntax.Operator"
_Operator_backtick = Core.Name "backtick"
_Operator_normal = Core.Name "normal"
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
  -- | A record pattern
  PatternRecord RecordPattern |
  -- | A tuple pattern
  PatternTuple [Pattern] |
  -- | A typed pattern
  PatternTyped TypedPattern |
  -- | A wildcard pattern
  PatternWildcard
  deriving (Eq, Ord, Read, Show)
_Pattern = Core.Name "hydra.haskell.syntax.Pattern"
_Pattern_application = Core.Name "application"
_Pattern_as = Core.Name "as"
_Pattern_list = Core.Name "list"
_Pattern_literal = Core.Name "literal"
_Pattern_name = Core.Name "name"
_Pattern_record = Core.Name "record"
_Pattern_tuple = Core.Name "tuple"
_Pattern_typed = Core.Name "typed"
_Pattern_wildcard = Core.Name "wildcard"
-- | An application pattern
data ApplicationPattern =
  ApplicationPattern {
    -- | The constructor name
    applicationPatternName :: Name,
    -- | The pattern arguments
    applicationPatternArgs :: [Pattern]}
  deriving (Eq, Ord, Read, Show)
_ApplicationPattern = Core.Name "hydra.haskell.syntax.ApplicationPattern"
_ApplicationPattern_name = Core.Name "name"
_ApplicationPattern_args = Core.Name "args"
-- | An 'as' pattern
data AsPattern =
  AsPattern {
    -- | The bound name
    asPatternName :: Name,
    -- | The inner pattern
    asPatternInner :: Pattern}
  deriving (Eq, Ord, Read, Show)
_AsPattern = Core.Name "hydra.haskell.syntax.AsPattern"
_AsPattern_name = Core.Name "name"
_AsPattern_inner = Core.Name "inner"
-- | A record pattern
data RecordPattern =
  RecordPattern {
    -- | The constructor name
    recordPatternName :: Name,
    -- | The field patterns
    recordPatternFields :: [PatternField]}
  deriving (Eq, Ord, Read, Show)
_RecordPattern = Core.Name "hydra.haskell.syntax.RecordPattern"
_RecordPattern_name = Core.Name "name"
_RecordPattern_fields = Core.Name "fields"
-- | A typed pattern
data TypedPattern =
  TypedPattern {
    -- | The inner pattern
    typedPatternInner :: Pattern,
    -- | The type annotation
    typedPatternType :: Type}
  deriving (Eq, Ord, Read, Show)
_TypedPattern = Core.Name "hydra.haskell.syntax.TypedPattern"
_TypedPattern_inner = Core.Name "inner"
_TypedPattern_type = Core.Name "type"
-- | A pattern field
data PatternField =
  PatternField {
    -- | The field name
    patternFieldName :: Name,
    -- | The field pattern
    patternFieldPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)
_PatternField = Core.Name "hydra.haskell.syntax.PatternField"
_PatternField_name = Core.Name "name"
_PatternField_pattern = Core.Name "pattern"
-- | A qualified name
data QualifiedName =
  QualifiedName {
    -- | The qualifier parts
    qualifiedNameQualifiers :: [NamePart],
    -- | The unqualified name part
    qualifiedNameUnqualified :: NamePart}
  deriving (Eq, Ord, Read, Show)
_QualifiedName = Core.Name "hydra.haskell.syntax.QualifiedName"
_QualifiedName_qualifiers = Core.Name "qualifiers"
_QualifiedName_unqualified = Core.Name "unqualified"
-- | A right-hand side of a binding
newtype RightHandSide =
  RightHandSide {
    unRightHandSide :: Expression}
  deriving (Eq, Ord, Read, Show)
_RightHandSide = Core.Name "hydra.haskell.syntax.RightHandSide"
-- | A do-notation statement
newtype Statement =
  Statement {
    unStatement :: Expression}
  deriving (Eq, Ord, Read, Show)
_Statement = Core.Name "hydra.haskell.syntax.Statement"
-- | A type expression
data Type =
  -- | An application type
  TypeApplication ApplicationType |
  -- | A context type
  TypeCtx ConstrainedType |
  -- | A function type
  TypeFunction FunctionType |
  -- | An infix type
  TypeInfix InfixType |
  -- | A list type
  TypeList Type |
  -- | A tuple type
  TypeTuple [Type] |
  -- | A type variable or type name
  TypeVariable Name
  deriving (Eq, Ord, Read, Show)
_Type = Core.Name "hydra.haskell.syntax.Type"
_Type_application = Core.Name "application"
_Type_ctx = Core.Name "ctx"
_Type_function = Core.Name "function"
_Type_infix = Core.Name "infix"
_Type_list = Core.Name "list"
_Type_tuple = Core.Name "tuple"
_Type_variable = Core.Name "variable"
-- | An application type
data ApplicationType =
  ApplicationType {
    -- | The type being applied
    applicationTypeContext :: Type,
    -- | The type argument
    applicationTypeArgument :: Type}
  deriving (Eq, Ord, Read, Show)
_ApplicationType = Core.Name "hydra.haskell.syntax.ApplicationType"
_ApplicationType_context = Core.Name "context"
_ApplicationType_argument = Core.Name "argument"
-- | A type with a context (type class constraints)
data ConstrainedType =
  ConstrainedType {
    -- | The type class context
    constrainedTypeCtx :: Constraint,
    -- | The constrained type
    constrainedTypeType :: Type}
  deriving (Eq, Ord, Read, Show)
_ConstrainedType = Core.Name "hydra.haskell.syntax.ConstrainedType"
_ConstrainedType_ctx = Core.Name "ctx"
_ConstrainedType_type = Core.Name "type"
-- | A function type
data FunctionType =
  FunctionType {
    -- | The domain type
    functionTypeDomain :: Type,
    -- | The codomain type
    functionTypeCodomain :: Type}
  deriving (Eq, Ord, Read, Show)
_FunctionType = Core.Name "hydra.haskell.syntax.FunctionType"
_FunctionType_domain = Core.Name "domain"
_FunctionType_codomain = Core.Name "codomain"
-- | An infix type application
data InfixType =
  InfixType {
    -- | The left-hand type
    infixTypeLhs :: Type,
    -- | The type operator
    infixTypeOperator :: Operator,
    -- | The right-hand type
    infixTypeRhs :: Type}
  deriving (Eq, Ord, Read, Show)
_InfixType = Core.Name "hydra.haskell.syntax.InfixType"
_InfixType_lhs = Core.Name "lhs"
_InfixType_operator = Core.Name "operator"
_InfixType_rhs = Core.Name "rhs"
-- | A type synonym declaration
data TypeSynonymDeclaration =
  TypeSynonymDeclaration {
    -- | The declaration head
    typeSynonymDeclarationName :: DeclarationHead,
    -- | The type being defined
    typeSynonymDeclarationType :: Type,
    -- | Optional comments
    typeSynonymDeclarationComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_TypeSynonymDeclaration = Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"
_TypeSynonymDeclaration_name = Core.Name "name"
_TypeSynonymDeclaration_type = Core.Name "type"
_TypeSynonymDeclaration_comments = Core.Name "comments"
-- | A type signature
data TypeSignature =
  TypeSignature {
    -- | The name being typed
    typeSignatureName :: Name,
    -- | The type
    typeSignatureType :: Type}
  deriving (Eq, Ord, Read, Show)
_TypeSignature = Core.Name "hydra.haskell.syntax.TypeSignature"
_TypeSignature_name = Core.Name "name"
_TypeSignature_type = Core.Name "type"
-- | A binding with its type signature
data TypedBinding =
  TypedBinding {
    -- | The type signature
    typedBindingTypeSignature :: TypeSignature,
    -- | The value binding
    typedBindingValueBinding :: ValueBinding,
    -- | Optional comments
    typedBindingComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_TypedBinding = Core.Name "hydra.haskell.syntax.TypedBinding"
_TypedBinding_typeSignature = Core.Name "typeSignature"
_TypedBinding_valueBinding = Core.Name "valueBinding"
_TypedBinding_comments = Core.Name "comments"
-- | A value binding
data ValueBinding =
  -- | A simple value binding
  ValueBindingSimple SimpleValueBinding
  deriving (Eq, Ord, Read, Show)
_ValueBinding = Core.Name "hydra.haskell.syntax.ValueBinding"
_ValueBinding_simple = Core.Name "simple"
-- | A simple value binding
data SimpleValueBinding =
  SimpleValueBinding {
    -- | The pattern being bound
    simpleValueBindingPattern :: Pattern,
    -- | The right-hand side
    simpleValueBindingRhs :: RightHandSide,
    -- | Optional local bindings (where clause)
    simpleValueBindingLocalBindings :: (Maybe LocalBindings),
    -- | Optional comments
    simpleValueBindingComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_SimpleValueBinding = Core.Name "hydra.haskell.syntax.SimpleValueBinding"
_SimpleValueBinding_pattern = Core.Name "pattern"
_SimpleValueBinding_rhs = Core.Name "rhs"
_SimpleValueBinding_localBindings = Core.Name "localBindings"
_SimpleValueBinding_comments = Core.Name "comments"
-- | A type variable
newtype Variable =
  Variable {
    unVariable :: Name}
  deriving (Eq, Ord, Read, Show)
_Variable = Core.Name "hydra.haskell.syntax.Variable"
