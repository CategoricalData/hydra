module Hydra.Ext.Haskell.Ast where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- A pattern-matching alternative
data Alternative 
  = Alternative {
    alternativePattern :: Pattern,
    alternativeRhs :: CaseRhs,
    alternativeBinds :: (Maybe LocalBindings)}
  deriving (Eq, Ord, Read, Show)

_Alternative = "hydra/ext/haskell/ast.Alternative"

_Alternative_pattern = "pattern"

_Alternative_rhs = "rhs"

_Alternative_binds = "binds"

-- A type assertion
data Assertion 
  = Assertion {
    assertionName :: Name,
    assertionTypes :: [Type]}
  deriving (Eq, Ord, Read, Show)

_Assertion = "hydra/ext/haskell/ast.Assertion"

_Assertion_name = "name"

_Assertion_types = "types"

-- The right-hand side of a pattern-matching alternative
newtype CaseRhs 
  = CaseRhs Expression
  deriving (Eq, Ord, Read, Show)

_CaseRhs = "hydra/ext/haskell/ast.CaseRhs"

-- A data constructor
data Constructor 
  = ConstructorOrdinary Constructor_Ordinary
  | ConstructorRecord Constructor_Record
  deriving (Eq, Ord, Read, Show)

_Constructor = "hydra/ext/haskell/ast.Constructor"

_Constructor_ordinary = "ordinary"

_Constructor_record = "record"

-- An ordinary (positional) data constructor
data Constructor_Ordinary 
  = Constructor_Ordinary {
    constructor_OrdinaryName :: Name,
    constructor_OrdinaryFields :: [Type]}
  deriving (Eq, Ord, Read, Show)

_Constructor_Ordinary = "hydra/ext/haskell/ast.Constructor.Ordinary"

_Constructor_Ordinary_name = "name"

_Constructor_Ordinary_fields = "fields"

-- A record-style data constructor
data Constructor_Record 
  = Constructor_Record {
    constructor_RecordName :: Name,
    constructor_RecordFields :: [Field]}
  deriving (Eq, Ord, Read, Show)

_Constructor_Record = "hydra/ext/haskell/ast.Constructor.Record"

_Constructor_Record_name = "name"

_Constructor_Record_fields = "fields"

-- A data type declaration
data DataDeclaration 
  = DataDeclaration {
    dataDeclarationKeyword :: DataDeclaration_Keyword,
    dataDeclarationContext :: [Assertion],
    dataDeclarationHead :: DeclarationHead,
    dataDeclarationConstructors :: [Constructor],
    dataDeclarationDeriving :: [Deriving]}
  deriving (Eq, Ord, Read, Show)

_DataDeclaration = "hydra/ext/haskell/ast.DataDeclaration"

_DataDeclaration_keyword = "keyword"

_DataDeclaration_context = "context"

_DataDeclaration_head = "head"

_DataDeclaration_constructors = "constructors"

_DataDeclaration_deriving = "deriving"

-- The 'data' versus 'newtype keyword
data DataDeclaration_Keyword 
  = DataDeclaration_KeywordData 
  | DataDeclaration_KeywordNewtype 
  deriving (Eq, Ord, Read, Show)

_DataDeclaration_Keyword = "hydra/ext/haskell/ast.DataDeclaration.Keyword"

_DataDeclaration_Keyword_data = "data"

_DataDeclaration_Keyword_newtype = "newtype"

-- A data declaration together with any comments
data DeclarationWithComments 
  = DeclarationWithComments {
    declarationWithCommentsBody :: Declaration,
    declarationWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_DeclarationWithComments = "hydra/ext/haskell/ast.DeclarationWithComments"

_DeclarationWithComments_body = "body"

_DeclarationWithComments_comments = "comments"

-- A data or value declaration
data Declaration 
  = DeclarationData DataDeclaration
  | DeclarationType TypeDeclaration
  | DeclarationValueBinding ValueBinding
  | DeclarationTypedBinding TypedBinding
  deriving (Eq, Ord, Read, Show)

_Declaration = "hydra/ext/haskell/ast.Declaration"

_Declaration_data = "data"

_Declaration_type = "type"

_Declaration_valueBinding = "valueBinding"

_Declaration_typedBinding = "typedBinding"

-- The left-hand side of a declaration
data DeclarationHead 
  = DeclarationHeadApplication DeclarationHead_Application
  | DeclarationHeadParens DeclarationHead
  | DeclarationHeadSimple Name
  deriving (Eq, Ord, Read, Show)

_DeclarationHead = "hydra/ext/haskell/ast.DeclarationHead"

_DeclarationHead_application = "application"

_DeclarationHead_parens = "parens"

_DeclarationHead_simple = "simple"

-- An application-style declaration head
data DeclarationHead_Application 
  = DeclarationHead_Application {
    declarationHead_ApplicationFunction :: DeclarationHead,
    declarationHead_ApplicationOperand :: Variable}
  deriving (Eq, Ord, Read, Show)

_DeclarationHead_Application = "hydra/ext/haskell/ast.DeclarationHead.Application"

_DeclarationHead_Application_function = "function"

_DeclarationHead_Application_operand = "operand"

-- A 'deriving' statement
newtype Deriving 
  = Deriving [Name]
  deriving (Eq, Ord, Read, Show)

_Deriving = "hydra/ext/haskell/ast.Deriving"

-- An export statement
data Export 
  = ExportDeclaration ImportExportSpec
  | ExportModule ModuleName
  deriving (Eq, Ord, Read, Show)

_Export = "hydra/ext/haskell/ast.Export"

_Export_declaration = "declaration"

_Export_module = "module"

-- A data expression
data Expression 
  = ExpressionApplication Expression_Application
  | ExpressionCase Expression_Case
  | ExpressionConstructRecord Expression_ConstructRecord
  | ExpressionDo [Statement]
  | ExpressionIf Expression_If
  | ExpressionInfixApplication Expression_InfixApplication
  | ExpressionLiteral Literal
  | ExpressionLambda Expression_Lambda
  | ExpressionLeftSection Expression_Section
  | ExpressionLet Expression_Let
  | ExpressionList [Expression]
  | ExpressionParens Expression
  | ExpressionPrefixApplication Expression_PrefixApplication
  | ExpressionRightSection Expression_Section
  | ExpressionTuple [Expression]
  | ExpressionTypeSignature Expression_TypeSignature
  | ExpressionUpdateRecord Expression_UpdateRecord
  | ExpressionVariable Name
  deriving (Eq, Ord, Read, Show)

_Expression = "hydra/ext/haskell/ast.Expression"

_Expression_application = "application"

_Expression_case = "case"

_Expression_constructRecord = "constructRecord"

_Expression_do = "do"

_Expression_if = "if"

_Expression_infixApplication = "infixApplication"

_Expression_literal = "literal"

_Expression_lambda = "lambda"

_Expression_leftSection = "leftSection"

_Expression_let = "let"

_Expression_list = "list"

_Expression_parens = "parens"

_Expression_prefixApplication = "prefixApplication"

_Expression_rightSection = "rightSection"

_Expression_tuple = "tuple"

_Expression_typeSignature = "typeSignature"

_Expression_updateRecord = "updateRecord"

_Expression_variable = "variable"

-- An application expression
data Expression_Application 
  = Expression_Application {
    expression_ApplicationFunction :: Expression,
    expression_ApplicationArgument :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_Application = "hydra/ext/haskell/ast.Expression.Application"

_Expression_Application_function = "function"

_Expression_Application_argument = "argument"

-- A case expression
data Expression_Case 
  = Expression_Case {
    expression_CaseCase :: Expression,
    expression_CaseAlternatives :: [Alternative]}
  deriving (Eq, Ord, Read, Show)

_Expression_Case = "hydra/ext/haskell/ast.Expression.Case"

_Expression_Case_case = "case"

_Expression_Case_alternatives = "alternatives"

-- A record constructor expression
data Expression_ConstructRecord 
  = Expression_ConstructRecord {
    expression_ConstructRecordName :: Name,
    expression_ConstructRecordFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)

_Expression_ConstructRecord = "hydra/ext/haskell/ast.Expression.ConstructRecord"

_Expression_ConstructRecord_name = "name"

_Expression_ConstructRecord_fields = "fields"

-- An 'if' expression
data Expression_If 
  = Expression_If {
    expression_IfCondition :: Expression,
    expression_IfThen :: Expression,
    expression_IfElse :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_If = "hydra/ext/haskell/ast.Expression.If"

_Expression_If_condition = "condition"

_Expression_If_then = "then"

_Expression_If_else = "else"

-- An infix application expression
data Expression_InfixApplication 
  = Expression_InfixApplication {
    expression_InfixApplicationLhs :: Expression,
    expression_InfixApplicationOperator :: Operator,
    expression_InfixApplicationRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_InfixApplication = "hydra/ext/haskell/ast.Expression.InfixApplication"

_Expression_InfixApplication_lhs = "lhs"

_Expression_InfixApplication_operator = "operator"

_Expression_InfixApplication_rhs = "rhs"

-- A lambda expression
data Expression_Lambda 
  = Expression_Lambda {
    expression_LambdaBindings :: [Pattern],
    expression_LambdaInner :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_Lambda = "hydra/ext/haskell/ast.Expression.Lambda"

_Expression_Lambda_bindings = "bindings"

_Expression_Lambda_inner = "inner"

-- A 'let' expression
data Expression_Let 
  = Expression_Let {
    expression_LetBindings :: [Pattern],
    expression_LetInner :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_Let = "hydra/ext/haskell/ast.Expression.Let"

_Expression_Let_bindings = "bindings"

_Expression_Let_inner = "inner"

-- A prefix expression
data Expression_PrefixApplication 
  = Expression_PrefixApplication {
    expression_PrefixApplicationOperator :: Operator,
    expression_PrefixApplicationRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_PrefixApplication = "hydra/ext/haskell/ast.Expression.PrefixApplication"

_Expression_PrefixApplication_operator = "operator"

_Expression_PrefixApplication_rhs = "rhs"

-- A section expression
data Expression_Section 
  = Expression_Section {
    expression_SectionOperator :: Operator,
    expression_SectionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_Section = "hydra/ext/haskell/ast.Expression.Section"

_Expression_Section_operator = "operator"

_Expression_Section_expression = "expression"

-- A type signature expression
data Expression_TypeSignature 
  = Expression_TypeSignature {
    expression_TypeSignatureInner :: Expression,
    expression_TypeSignatureType :: Type}
  deriving (Eq, Ord, Read, Show)

_Expression_TypeSignature = "hydra/ext/haskell/ast.Expression.TypeSignature"

_Expression_TypeSignature_inner = "inner"

_Expression_TypeSignature_type = "type"

-- An update record expression
data Expression_UpdateRecord 
  = Expression_UpdateRecord {
    expression_UpdateRecordInner :: Expression,
    expression_UpdateRecordFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)

_Expression_UpdateRecord = "hydra/ext/haskell/ast.Expression.UpdateRecord"

_Expression_UpdateRecord_inner = "inner"

_Expression_UpdateRecord_fields = "fields"

-- A field (name/type pair)
data Field 
  = Field {
    fieldName :: Name,
    fieldType :: Type}
  deriving (Eq, Ord, Read, Show)

_Field = "hydra/ext/haskell/ast.Field"

_Field_name = "name"

_Field_type = "type"

-- A field name and value
data FieldUpdate 
  = FieldUpdate {
    fieldUpdateName :: Name,
    fieldUpdateValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_FieldUpdate = "hydra/ext/haskell/ast.FieldUpdate"

_FieldUpdate_name = "name"

_FieldUpdate_value = "value"

-- An import statement
data Import 
  = Import {
    importQualified :: Bool,
    importModule :: ModuleName,
    importAs :: (Maybe ModuleName),
    importSpec :: (Maybe Import_Spec)}
  deriving (Eq, Ord, Read, Show)

_Import = "hydra/ext/haskell/ast.Import"

_Import_qualified = "qualified"

_Import_module = "module"

_Import_as = "as"

_Import_spec = "spec"

-- An import specification
data Import_Spec 
  = Import_SpecList [ImportExportSpec]
  | Import_SpecHiding [ImportExportSpec]
  deriving (Eq, Ord, Read, Show)

_Import_Spec = "hydra/ext/haskell/ast.Import.Spec"

_Import_Spec_list = "list"

_Import_Spec_hiding = "hiding"

-- An import modifier ('pattern' or 'type')
data ImportModifier 
  = ImportModifierPattern 
  | ImportModifierType 
  deriving (Eq, Ord, Read, Show)

_ImportModifier = "hydra/ext/haskell/ast.ImportModifier"

_ImportModifier_pattern = "pattern"

_ImportModifier_type = "type"

-- An import or export specification
data ImportExportSpec 
  = ImportExportSpec {
    importExportSpecModifier :: (Maybe ImportModifier),
    importExportSpecName :: Name,
    importExportSpecSubspec :: (Maybe ImportExportSpec_Subspec)}
  deriving (Eq, Ord, Read, Show)

_ImportExportSpec = "hydra/ext/haskell/ast.ImportExportSpec"

_ImportExportSpec_modifier = "modifier"

_ImportExportSpec_name = "name"

_ImportExportSpec_subspec = "subspec"

data ImportExportSpec_Subspec 
  = ImportExportSpec_SubspecAll 
  | ImportExportSpec_SubspecList [Name]
  deriving (Eq, Ord, Read, Show)

_ImportExportSpec_Subspec = "hydra/ext/haskell/ast.ImportExportSpec.Subspec"

_ImportExportSpec_Subspec_all = "all"

_ImportExportSpec_Subspec_list = "list"

-- A literal value
data Literal 
  = LiteralChar Int
  | LiteralDouble Double
  | LiteralFloat Float
  | LiteralInt Int
  | LiteralInteger Integer
  | LiteralString String
  deriving (Eq, Ord, Read, Show)

_Literal = "hydra/ext/haskell/ast.Literal"

_Literal_char = "char"

_Literal_double = "double"

_Literal_float = "float"

_Literal_int = "int"

_Literal_integer = "integer"

_Literal_string = "string"

data LocalBinding 
  = LocalBindingSignature TypeSignature
  | LocalBindingValue ValueBinding
  deriving (Eq, Ord, Read, Show)

_LocalBinding = "hydra/ext/haskell/ast.LocalBinding"

_LocalBinding_signature = "signature"

_LocalBinding_value = "value"

newtype LocalBindings 
  = LocalBindings [LocalBinding]
  deriving (Eq, Ord, Read, Show)

_LocalBindings = "hydra/ext/haskell/ast.LocalBindings"

data Module 
  = Module {
    moduleHead :: (Maybe ModuleHead),
    moduleImports :: [Import],
    moduleDeclarations :: [DeclarationWithComments]}
  deriving (Eq, Ord, Read, Show)

_Module = "hydra/ext/haskell/ast.Module"

_Module_head = "head"

_Module_imports = "imports"

_Module_declarations = "declarations"

data ModuleHead 
  = ModuleHead {
    moduleHeadName :: ModuleName,
    moduleHeadExports :: [Export]}
  deriving (Eq, Ord, Read, Show)

_ModuleHead = "hydra/ext/haskell/ast.ModuleHead"

_ModuleHead_name = "name"

_ModuleHead_exports = "exports"

newtype ModuleName 
  = ModuleName String
  deriving (Eq, Ord, Read, Show)

_ModuleName = "hydra/ext/haskell/ast.ModuleName"

data Name 
  = NameImplicit QualifiedName
  | NameNormal QualifiedName
  | NameParens QualifiedName
  deriving (Eq, Ord, Read, Show)

_Name = "hydra/ext/haskell/ast.Name"

_Name_implicit = "implicit"

_Name_normal = "normal"

_Name_parens = "parens"

newtype NamePart 
  = NamePart String
  deriving (Eq, Ord, Read, Show)

_NamePart = "hydra/ext/haskell/ast.NamePart"

data Operator 
  = OperatorBacktick QualifiedName
  | OperatorNormal QualifiedName
  deriving (Eq, Ord, Read, Show)

_Operator = "hydra/ext/haskell/ast.Operator"

_Operator_backtick = "backtick"

_Operator_normal = "normal"

data Pattern 
  = PatternApplication Pattern_Application
  | PatternAs Pattern_As
  | PatternList [Pattern]
  | PatternLiteral Literal
  | PatternName Name
  | PatternParens Pattern
  | PatternRecord Pattern_Record
  | PatternTuple [Pattern]
  | PatternTyped Pattern_Typed
  | PatternWildcard 
  deriving (Eq, Ord, Read, Show)

_Pattern = "hydra/ext/haskell/ast.Pattern"

_Pattern_application = "application"

_Pattern_as = "as"

_Pattern_list = "list"

_Pattern_literal = "literal"

_Pattern_name = "name"

_Pattern_parens = "parens"

_Pattern_record = "record"

_Pattern_tuple = "tuple"

_Pattern_typed = "typed"

_Pattern_wildcard = "wildcard"

data Pattern_Application 
  = Pattern_Application {
    pattern_ApplicationName :: Name,
    pattern_ApplicationArgs :: [Pattern]}
  deriving (Eq, Ord, Read, Show)

_Pattern_Application = "hydra/ext/haskell/ast.Pattern.Application"

_Pattern_Application_name = "name"

_Pattern_Application_args = "args"

data Pattern_As 
  = Pattern_As {
    pattern_AsName :: Name,
    pattern_AsInner :: Pattern}
  deriving (Eq, Ord, Read, Show)

_Pattern_As = "hydra/ext/haskell/ast.Pattern.As"

_Pattern_As_name = "name"

_Pattern_As_inner = "inner"

data Pattern_Record 
  = Pattern_Record {
    pattern_RecordName :: Name,
    pattern_RecordFields :: [PatternField]}
  deriving (Eq, Ord, Read, Show)

_Pattern_Record = "hydra/ext/haskell/ast.Pattern.Record"

_Pattern_Record_name = "name"

_Pattern_Record_fields = "fields"

data Pattern_Typed 
  = Pattern_Typed {
    pattern_TypedInner :: Pattern,
    pattern_TypedType :: Type}
  deriving (Eq, Ord, Read, Show)

_Pattern_Typed = "hydra/ext/haskell/ast.Pattern.Typed"

_Pattern_Typed_inner = "inner"

_Pattern_Typed_type = "type"

data PatternField 
  = PatternField {
    patternFieldName :: Name,
    patternFieldPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_PatternField = "hydra/ext/haskell/ast.PatternField"

_PatternField_name = "name"

_PatternField_pattern = "pattern"

data QualifiedName 
  = QualifiedName {
    qualifiedNameQualifiers :: [NamePart],
    qualifiedNameUnqualified :: NamePart}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = "hydra/ext/haskell/ast.QualifiedName"

_QualifiedName_qualifiers = "qualifiers"

_QualifiedName_unqualified = "unqualified"

newtype RightHandSide 
  = RightHandSide Expression
  deriving (Eq, Ord, Read, Show)

_RightHandSide = "hydra/ext/haskell/ast.RightHandSide"

newtype Statement 
  = Statement Expression
  deriving (Eq, Ord, Read, Show)

_Statement = "hydra/ext/haskell/ast.Statement"

data Type 
  = TypeApplication Type_Application
  | TypeFunction Type_Function
  | TypeInfix Type_Infix
  | TypeList Type
  | TypeParens Type
  | TypeTuple [Type]
  | TypeVariable Name
  deriving (Eq, Ord, Read, Show)

_Type = "hydra/ext/haskell/ast.Type"

_Type_application = "application"

_Type_function = "function"

_Type_infix = "infix"

_Type_list = "list"

_Type_parens = "parens"

_Type_tuple = "tuple"

_Type_variable = "variable"

data Type_Application 
  = Type_Application {
    type_ApplicationContext :: Type,
    type_ApplicationArgument :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Application = "hydra/ext/haskell/ast.Type.Application"

_Type_Application_context = "context"

_Type_Application_argument = "argument"

data Type_Function 
  = Type_Function {
    type_FunctionDomain :: Type,
    type_FunctionCodomain :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Function = "hydra/ext/haskell/ast.Type.Function"

_Type_Function_domain = "domain"

_Type_Function_codomain = "codomain"

data Type_Infix 
  = Type_Infix {
    type_InfixLhs :: Type,
    type_InfixOperator :: Operator,
    type_InfixRhs :: Operator}
  deriving (Eq, Ord, Read, Show)

_Type_Infix = "hydra/ext/haskell/ast.Type.Infix"

_Type_Infix_lhs = "lhs"

_Type_Infix_operator = "operator"

_Type_Infix_rhs = "rhs"

data TypeDeclaration 
  = TypeDeclaration {
    typeDeclarationName :: DeclarationHead,
    typeDeclarationType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeDeclaration = "hydra/ext/haskell/ast.TypeDeclaration"

_TypeDeclaration_name = "name"

_TypeDeclaration_type = "type"

data TypeSignature 
  = TypeSignature {
    typeSignatureName :: Name,
    typeSignatureType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeSignature = "hydra/ext/haskell/ast.TypeSignature"

_TypeSignature_name = "name"

_TypeSignature_type = "type"

data TypedBinding 
  = TypedBinding {
    typedBindingTypeSignature :: TypeSignature,
    typedBindingValueBinding :: ValueBinding}
  deriving (Eq, Ord, Read, Show)

_TypedBinding = "hydra/ext/haskell/ast.TypedBinding"

_TypedBinding_typeSignature = "typeSignature"

_TypedBinding_valueBinding = "valueBinding"

data ValueBinding 
  = ValueBindingSimple ValueBinding_Simple
  deriving (Eq, Ord, Read, Show)

_ValueBinding = "hydra/ext/haskell/ast.ValueBinding"

_ValueBinding_simple = "simple"

data ValueBinding_Simple 
  = ValueBinding_Simple {
    valueBinding_SimplePattern :: Pattern,
    valueBinding_SimpleRhs :: RightHandSide,
    valueBinding_SimpleLocalBindings :: (Maybe LocalBindings)}
  deriving (Eq, Ord, Read, Show)

_ValueBinding_Simple = "hydra/ext/haskell/ast.ValueBinding.Simple"

_ValueBinding_Simple_pattern = "pattern"

_ValueBinding_Simple_rhs = "rhs"

_ValueBinding_Simple_localBindings = "localBindings"

newtype Variable 
  = Variable Name
  deriving (Eq, Ord, Read, Show)

_Variable = "hydra/ext/haskell/ast.Variable"