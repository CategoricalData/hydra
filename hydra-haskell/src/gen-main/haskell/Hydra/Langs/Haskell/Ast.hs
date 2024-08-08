-- | A Haskell syntax model, loosely based on Language.Haskell.Tools.AST

module Hydra.Langs.Haskell.Ast where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A pattern-matching alternative
data Alternative = 
  Alternative {
    alternativePattern :: Pattern,
    alternativeRhs :: CaseRhs,
    alternativeBinds :: (Maybe LocalBindings)}
  deriving (Eq, Ord, Read, Show)

_Alternative = (Core.Name "hydra/langs/haskell/ast.Alternative")

_Alternative_pattern = (Core.Name "pattern")

_Alternative_rhs = (Core.Name "rhs")

_Alternative_binds = (Core.Name "binds")

_Alternative_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Alternative"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _CaseRhs_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "binds"),
      Core.fieldTypeType = (Core.TypeOptional _LocalBindings_type_)}]}))

-- | A type assertion
data Assertion = 
  AssertionClass Assertion_Class |
  AssertionTuple [Assertion]
  deriving (Eq, Ord, Read, Show)

_Assertion = (Core.Name "hydra/langs/haskell/ast.Assertion")

_Assertion_class = (Core.Name "class")

_Assertion_tuple = (Core.Name "tuple")

_Assertion_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Assertion"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _Assertion_Class_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tuple"),
      Core.fieldTypeType = (Core.TypeList _Assertion_type_)}]}))

data Assertion_Class = 
  Assertion_Class {
    assertion_ClassName :: Name,
    assertion_ClassTypes :: [Type]}
  deriving (Eq, Ord, Read, Show)

_Assertion_Class = (Core.Name "hydra/langs/haskell/ast.Assertion.Class")

_Assertion_Class_name = (Core.Name "name")

_Assertion_Class_types = (Core.Name "types")

_Assertion_Class_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Assertion.Class"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "types"),
      Core.fieldTypeType = (Core.TypeList _Type_type_)}]}))

-- | The right-hand side of a pattern-matching alternative
newtype CaseRhs = 
  CaseRhs {
    unCaseRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_CaseRhs = (Core.Name "hydra/langs/haskell/ast.CaseRhs")

_CaseRhs_type_ = _Expression_type_

-- | A data constructor
data Constructor = 
  ConstructorOrdinary Constructor_Ordinary |
  ConstructorRecord Constructor_Record
  deriving (Eq, Ord, Read, Show)

_Constructor = (Core.Name "hydra/langs/haskell/ast.Constructor")

_Constructor_ordinary = (Core.Name "ordinary")

_Constructor_record = (Core.Name "record")

_Constructor_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Constructor"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ordinary"),
      Core.fieldTypeType = _Constructor_Ordinary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "record"),
      Core.fieldTypeType = _Constructor_Record_type_}]}))

-- | An ordinary (positional) data constructor
data Constructor_Ordinary = 
  Constructor_Ordinary {
    constructor_OrdinaryName :: Name,
    constructor_OrdinaryFields :: [Type]}
  deriving (Eq, Ord, Read, Show)

_Constructor_Ordinary = (Core.Name "hydra/langs/haskell/ast.Constructor.Ordinary")

_Constructor_Ordinary_name = (Core.Name "name")

_Constructor_Ordinary_fields = (Core.Name "fields")

_Constructor_Ordinary_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Constructor.Ordinary"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fields"),
      Core.fieldTypeType = (Core.TypeList _Type_type_)}]}))

-- | A record-style data constructor
data Constructor_Record = 
  Constructor_Record {
    constructor_RecordName :: Name,
    constructor_RecordFields :: [FieldWithComments]}
  deriving (Eq, Ord, Read, Show)

_Constructor_Record = (Core.Name "hydra/langs/haskell/ast.Constructor.Record")

_Constructor_Record_name = (Core.Name "name")

_Constructor_Record_fields = (Core.Name "fields")

_Constructor_Record_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Constructor.Record"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fields"),
      Core.fieldTypeType = (Core.TypeList _FieldWithComments_type_)}]}))

-- | A data constructor together with any comments
data ConstructorWithComments = 
  ConstructorWithComments {
    constructorWithCommentsBody :: Constructor,
    constructorWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_ConstructorWithComments = (Core.Name "hydra/langs/haskell/ast.ConstructorWithComments")

_ConstructorWithComments_body = (Core.Name "body")

_ConstructorWithComments_comments = (Core.Name "comments")

_ConstructorWithComments_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.ConstructorWithComments"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _Constructor_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "comments"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))}]}))

-- | A data type declaration
data DataDeclaration = 
  DataDeclaration {
    dataDeclarationKeyword :: DataDeclaration_Keyword,
    dataDeclarationContext :: [Assertion],
    dataDeclarationHead :: DeclarationHead,
    dataDeclarationConstructors :: [ConstructorWithComments],
    dataDeclarationDeriving :: [Deriving]}
  deriving (Eq, Ord, Read, Show)

_DataDeclaration = (Core.Name "hydra/langs/haskell/ast.DataDeclaration")

_DataDeclaration_keyword = (Core.Name "keyword")

_DataDeclaration_context = (Core.Name "context")

_DataDeclaration_head = (Core.Name "head")

_DataDeclaration_constructors = (Core.Name "constructors")

_DataDeclaration_deriving = (Core.Name "deriving")

_DataDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.DataDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "keyword"),
      Core.fieldTypeType = _DataDeclaration_Keyword_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "context"),
      Core.fieldTypeType = (Core.TypeList _Assertion_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "head"),
      Core.fieldTypeType = _DeclarationHead_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constructors"),
      Core.fieldTypeType = (Core.TypeList _ConstructorWithComments_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "deriving"),
      Core.fieldTypeType = (Core.TypeList _Deriving_type_)}]}))

-- | The 'data' versus 'newtype keyword
data DataDeclaration_Keyword = 
  DataDeclaration_KeywordData  |
  DataDeclaration_KeywordNewtype 
  deriving (Eq, Ord, Read, Show)

_DataDeclaration_Keyword = (Core.Name "hydra/langs/haskell/ast.DataDeclaration.Keyword")

_DataDeclaration_Keyword_data = (Core.Name "data")

_DataDeclaration_Keyword_newtype = (Core.Name "newtype")

_DataDeclaration_Keyword_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.DataDeclaration.Keyword"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "data"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "newtype"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | A data declaration together with any comments
data DeclarationWithComments = 
  DeclarationWithComments {
    declarationWithCommentsBody :: Declaration,
    declarationWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_DeclarationWithComments = (Core.Name "hydra/langs/haskell/ast.DeclarationWithComments")

_DeclarationWithComments_body = (Core.Name "body")

_DeclarationWithComments_comments = (Core.Name "comments")

_DeclarationWithComments_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.DeclarationWithComments"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _Declaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "comments"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))}]}))

-- | A data or value declaration
data Declaration = 
  DeclarationData DataDeclaration |
  DeclarationType TypeDeclaration |
  DeclarationValueBinding ValueBinding |
  DeclarationTypedBinding TypedBinding
  deriving (Eq, Ord, Read, Show)

_Declaration = (Core.Name "hydra/langs/haskell/ast.Declaration")

_Declaration_data = (Core.Name "data")

_Declaration_type = (Core.Name "type")

_Declaration_valueBinding = (Core.Name "valueBinding")

_Declaration_typedBinding = (Core.Name "typedBinding")

_Declaration_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Declaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "data"),
      Core.fieldTypeType = _DataDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _TypeDeclaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "valueBinding"),
      Core.fieldTypeType = _ValueBinding_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typedBinding"),
      Core.fieldTypeType = _TypedBinding_type_}]}))

-- | The left-hand side of a declaration
data DeclarationHead = 
  DeclarationHeadApplication DeclarationHead_Application |
  DeclarationHeadParens DeclarationHead |
  DeclarationHeadSimple Name
  deriving (Eq, Ord, Read, Show)

_DeclarationHead = (Core.Name "hydra/langs/haskell/ast.DeclarationHead")

_DeclarationHead_application = (Core.Name "application")

_DeclarationHead_parens = (Core.Name "parens")

_DeclarationHead_simple = (Core.Name "simple")

_DeclarationHead_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.DeclarationHead"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "application"),
      Core.fieldTypeType = _DeclarationHead_Application_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parens"),
      Core.fieldTypeType = _DeclarationHead_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = _Name_type_}]}))

-- | An application-style declaration head
data DeclarationHead_Application = 
  DeclarationHead_Application {
    declarationHead_ApplicationFunction :: DeclarationHead,
    declarationHead_ApplicationOperand :: Variable}
  deriving (Eq, Ord, Read, Show)

_DeclarationHead_Application = (Core.Name "hydra/langs/haskell/ast.DeclarationHead.Application")

_DeclarationHead_Application_function = (Core.Name "function")

_DeclarationHead_Application_operand = (Core.Name "operand")

_DeclarationHead_Application_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.DeclarationHead.Application"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "function"),
      Core.fieldTypeType = _DeclarationHead_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operand"),
      Core.fieldTypeType = _Variable_type_}]}))

-- | A 'deriving' statement
newtype Deriving = 
  Deriving {
    unDeriving :: [Name]}
  deriving (Eq, Ord, Read, Show)

_Deriving = (Core.Name "hydra/langs/haskell/ast.Deriving")

_Deriving_type_ = (Core.TypeList _Name_type_)

-- | An export statement
data Export = 
  ExportDeclaration ImportExportSpec |
  ExportModule ModuleName
  deriving (Eq, Ord, Read, Show)

_Export = (Core.Name "hydra/langs/haskell/ast.Export")

_Export_declaration = (Core.Name "declaration")

_Export_module = (Core.Name "module")

_Export_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Export"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "declaration"),
      Core.fieldTypeType = _ImportExportSpec_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "module"),
      Core.fieldTypeType = _ModuleName_type_}]}))

-- | A data expression
data Expression = 
  ExpressionApplication Expression_Application |
  ExpressionCase Expression_Case |
  ExpressionConstructRecord Expression_ConstructRecord |
  ExpressionDo [Statement] |
  ExpressionIf Expression_If |
  ExpressionInfixApplication Expression_InfixApplication |
  ExpressionLiteral Literal |
  ExpressionLambda Expression_Lambda |
  ExpressionLeftSection Expression_Section |
  ExpressionLet Expression_Let |
  ExpressionList [Expression] |
  ExpressionParens Expression |
  ExpressionPrefixApplication Expression_PrefixApplication |
  ExpressionRightSection Expression_Section |
  ExpressionTuple [Expression] |
  ExpressionTypeSignature Expression_TypeSignature |
  ExpressionUpdateRecord Expression_UpdateRecord |
  ExpressionVariable Name
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra/langs/haskell/ast.Expression")

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

_Expression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "application"),
      Core.fieldTypeType = _Expression_Application_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "case"),
      Core.fieldTypeType = _Expression_Case_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constructRecord"),
      Core.fieldTypeType = _Expression_ConstructRecord_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "do"),
      Core.fieldTypeType = (Core.TypeList _Statement_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "if"),
      Core.fieldTypeType = _Expression_If_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "infixApplication"),
      Core.fieldTypeType = _Expression_InfixApplication_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _Literal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lambda"),
      Core.fieldTypeType = _Expression_Lambda_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "leftSection"),
      Core.fieldTypeType = _Expression_Section_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "let"),
      Core.fieldTypeType = _Expression_Let_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parens"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "prefixApplication"),
      Core.fieldTypeType = _Expression_PrefixApplication_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rightSection"),
      Core.fieldTypeType = _Expression_Section_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tuple"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeSignature"),
      Core.fieldTypeType = _Expression_TypeSignature_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "updateRecord"),
      Core.fieldTypeType = _Expression_UpdateRecord_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Name_type_}]}))

-- | An application expression
data Expression_Application = 
  Expression_Application {
    expression_ApplicationFunction :: Expression,
    expression_ApplicationArgument :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_Application = (Core.Name "hydra/langs/haskell/ast.Expression.Application")

_Expression_Application_function = (Core.Name "function")

_Expression_Application_argument = (Core.Name "argument")

_Expression_Application_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.Application"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "function"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "argument"),
      Core.fieldTypeType = _Expression_type_}]}))

-- | A case expression
data Expression_Case = 
  Expression_Case {
    expression_CaseCase :: Expression,
    expression_CaseAlternatives :: [Alternative]}
  deriving (Eq, Ord, Read, Show)

_Expression_Case = (Core.Name "hydra/langs/haskell/ast.Expression.Case")

_Expression_Case_case = (Core.Name "case")

_Expression_Case_alternatives = (Core.Name "alternatives")

_Expression_Case_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.Case"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "case"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "alternatives"),
      Core.fieldTypeType = (Core.TypeList _Alternative_type_)}]}))

-- | A record constructor expression
data Expression_ConstructRecord = 
  Expression_ConstructRecord {
    expression_ConstructRecordName :: Name,
    expression_ConstructRecordFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)

_Expression_ConstructRecord = (Core.Name "hydra/langs/haskell/ast.Expression.ConstructRecord")

_Expression_ConstructRecord_name = (Core.Name "name")

_Expression_ConstructRecord_fields = (Core.Name "fields")

_Expression_ConstructRecord_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.ConstructRecord"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fields"),
      Core.fieldTypeType = (Core.TypeList _FieldUpdate_type_)}]}))

-- | An 'if' expression
data Expression_If = 
  Expression_If {
    expression_IfCondition :: Expression,
    expression_IfThen :: Expression,
    expression_IfElse :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_If = (Core.Name "hydra/langs/haskell/ast.Expression.If")

_Expression_If_condition = (Core.Name "condition")

_Expression_If_then = (Core.Name "then")

_Expression_If_else = (Core.Name "else")

_Expression_If_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.If"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "condition"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "then"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "else"),
      Core.fieldTypeType = _Expression_type_}]}))

-- | An infix application expression
data Expression_InfixApplication = 
  Expression_InfixApplication {
    expression_InfixApplicationLhs :: Expression,
    expression_InfixApplicationOperator :: Operator,
    expression_InfixApplicationRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_InfixApplication = (Core.Name "hydra/langs/haskell/ast.Expression.InfixApplication")

_Expression_InfixApplication_lhs = (Core.Name "lhs")

_Expression_InfixApplication_operator = (Core.Name "operator")

_Expression_InfixApplication_rhs = (Core.Name "rhs")

_Expression_InfixApplication_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.InfixApplication"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _Operator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _Expression_type_}]}))

-- | A lambda expression
data Expression_Lambda = 
  Expression_Lambda {
    expression_LambdaBindings :: [Pattern],
    expression_LambdaInner :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_Lambda = (Core.Name "hydra/langs/haskell/ast.Expression.Lambda")

_Expression_Lambda_bindings = (Core.Name "bindings")

_Expression_Lambda_inner = (Core.Name "inner")

_Expression_Lambda_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.Lambda"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bindings"),
      Core.fieldTypeType = (Core.TypeList _Pattern_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inner"),
      Core.fieldTypeType = _Expression_type_}]}))

-- | A 'let' expression
data Expression_Let = 
  Expression_Let {
    expression_LetBindings :: [LocalBinding],
    expression_LetInner :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_Let = (Core.Name "hydra/langs/haskell/ast.Expression.Let")

_Expression_Let_bindings = (Core.Name "bindings")

_Expression_Let_inner = (Core.Name "inner")

_Expression_Let_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.Let"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bindings"),
      Core.fieldTypeType = (Core.TypeList _LocalBinding_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inner"),
      Core.fieldTypeType = _Expression_type_}]}))

-- | A prefix expression
data Expression_PrefixApplication = 
  Expression_PrefixApplication {
    expression_PrefixApplicationOperator :: Operator,
    expression_PrefixApplicationRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_PrefixApplication = (Core.Name "hydra/langs/haskell/ast.Expression.PrefixApplication")

_Expression_PrefixApplication_operator = (Core.Name "operator")

_Expression_PrefixApplication_rhs = (Core.Name "rhs")

_Expression_PrefixApplication_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.PrefixApplication"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _Operator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _Expression_type_}]}))

-- | A section expression
data Expression_Section = 
  Expression_Section {
    expression_SectionOperator :: Operator,
    expression_SectionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_Expression_Section = (Core.Name "hydra/langs/haskell/ast.Expression.Section")

_Expression_Section_operator = (Core.Name "operator")

_Expression_Section_expression = (Core.Name "expression")

_Expression_Section_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.Section"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _Operator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_}]}))

-- | A type signature expression
data Expression_TypeSignature = 
  Expression_TypeSignature {
    expression_TypeSignatureInner :: Expression,
    expression_TypeSignatureType :: Type}
  deriving (Eq, Ord, Read, Show)

_Expression_TypeSignature = (Core.Name "hydra/langs/haskell/ast.Expression.TypeSignature")

_Expression_TypeSignature_inner = (Core.Name "inner")

_Expression_TypeSignature_type = (Core.Name "type")

_Expression_TypeSignature_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.TypeSignature"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inner"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _Type_type_}]}))

-- | An update record expression
data Expression_UpdateRecord = 
  Expression_UpdateRecord {
    expression_UpdateRecordInner :: Expression,
    expression_UpdateRecordFields :: [FieldUpdate]}
  deriving (Eq, Ord, Read, Show)

_Expression_UpdateRecord = (Core.Name "hydra/langs/haskell/ast.Expression.UpdateRecord")

_Expression_UpdateRecord_inner = (Core.Name "inner")

_Expression_UpdateRecord_fields = (Core.Name "fields")

_Expression_UpdateRecord_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Expression.UpdateRecord"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inner"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fields"),
      Core.fieldTypeType = (Core.TypeList _FieldUpdate_type_)}]}))

-- | A field (name/type pair)
data Field = 
  Field {
    fieldName :: Name,
    fieldType :: Type}
  deriving (Eq, Ord, Read, Show)

_Field = (Core.Name "hydra/langs/haskell/ast.Field")

_Field_name = (Core.Name "name")

_Field_type = (Core.Name "type")

_Field_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Field"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _Type_type_}]}))

-- | A field together with any comments
data FieldWithComments = 
  FieldWithComments {
    fieldWithCommentsField :: Field,
    fieldWithCommentsComments :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_FieldWithComments = (Core.Name "hydra/langs/haskell/ast.FieldWithComments")

_FieldWithComments_field = (Core.Name "field")

_FieldWithComments_comments = (Core.Name "comments")

_FieldWithComments_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.FieldWithComments"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "field"),
      Core.fieldTypeType = _Field_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "comments"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))}]}))

-- | A field name and value
data FieldUpdate = 
  FieldUpdate {
    fieldUpdateName :: Name,
    fieldUpdateValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_FieldUpdate = (Core.Name "hydra/langs/haskell/ast.FieldUpdate")

_FieldUpdate_name = (Core.Name "name")

_FieldUpdate_value = (Core.Name "value")

_FieldUpdate_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.FieldUpdate"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Expression_type_}]}))

-- | An import statement
data Import = 
  Import {
    importQualified :: Bool,
    importModule :: ModuleName,
    importAs :: (Maybe ModuleName),
    importSpec :: (Maybe Import_Spec)}
  deriving (Eq, Ord, Read, Show)

_Import = (Core.Name "hydra/langs/haskell/ast.Import")

_Import_qualified = (Core.Name "qualified")

_Import_module = (Core.Name "module")

_Import_as = (Core.Name "as")

_Import_spec = (Core.Name "spec")

_Import_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Import"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualified"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "module"),
      Core.fieldTypeType = _ModuleName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "as"),
      Core.fieldTypeType = (Core.TypeOptional _ModuleName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "spec"),
      Core.fieldTypeType = (Core.TypeOptional _Import_Spec_type_)}]}))

-- | An import specification
data Import_Spec = 
  Import_SpecList [ImportExportSpec] |
  Import_SpecHiding [ImportExportSpec]
  deriving (Eq, Ord, Read, Show)

_Import_Spec = (Core.Name "hydra/langs/haskell/ast.Import.Spec")

_Import_Spec_list = (Core.Name "list")

_Import_Spec_hiding = (Core.Name "hiding")

_Import_Spec_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Import.Spec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = (Core.TypeList _ImportExportSpec_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hiding"),
      Core.fieldTypeType = (Core.TypeList _ImportExportSpec_type_)}]}))

-- | An import modifier ('pattern' or 'type')
data ImportModifier = 
  ImportModifierPattern  |
  ImportModifierType 
  deriving (Eq, Ord, Read, Show)

_ImportModifier = (Core.Name "hydra/langs/haskell/ast.ImportModifier")

_ImportModifier_pattern = (Core.Name "pattern")

_ImportModifier_type = (Core.Name "type")

_ImportModifier_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.ImportModifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | An import or export specification
data ImportExportSpec = 
  ImportExportSpec {
    importExportSpecModifier :: (Maybe ImportModifier),
    importExportSpecName :: Name,
    importExportSpecSubspec :: (Maybe ImportExportSpec_Subspec)}
  deriving (Eq, Ord, Read, Show)

_ImportExportSpec = (Core.Name "hydra/langs/haskell/ast.ImportExportSpec")

_ImportExportSpec_modifier = (Core.Name "modifier")

_ImportExportSpec_name = (Core.Name "name")

_ImportExportSpec_subspec = (Core.Name "subspec")

_ImportExportSpec_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.ImportExportSpec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "modifier"),
      Core.fieldTypeType = (Core.TypeOptional _ImportModifier_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subspec"),
      Core.fieldTypeType = (Core.TypeOptional _ImportExportSpec_Subspec_type_)}]}))

data ImportExportSpec_Subspec = 
  ImportExportSpec_SubspecAll  |
  ImportExportSpec_SubspecList [Name]
  deriving (Eq, Ord, Read, Show)

_ImportExportSpec_Subspec = (Core.Name "hydra/langs/haskell/ast.ImportExportSpec.Subspec")

_ImportExportSpec_Subspec_all = (Core.Name "all")

_ImportExportSpec_Subspec_list = (Core.Name "list")

_ImportExportSpec_Subspec_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.ImportExportSpec.Subspec"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "all"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = (Core.TypeList _Name_type_)}]}))

-- | A literal value
data Literal = 
  LiteralChar Int |
  LiteralDouble Double |
  LiteralFloat Float |
  LiteralInt Int |
  LiteralInteger Integer |
  LiteralString String
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/langs/haskell/ast.Literal")

_Literal_char = (Core.Name "char")

_Literal_double = (Core.Name "double")

_Literal_float = (Core.Name "float")

_Literal_int = (Core.Name "int")

_Literal_integer = (Core.Name "integer")

_Literal_string = (Core.Name "string")

_Literal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Literal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "char"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "double"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

data LocalBinding = 
  LocalBindingSignature TypeSignature |
  LocalBindingValue ValueBinding
  deriving (Eq, Ord, Read, Show)

_LocalBinding = (Core.Name "hydra/langs/haskell/ast.LocalBinding")

_LocalBinding_signature = (Core.Name "signature")

_LocalBinding_value = (Core.Name "value")

_LocalBinding_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.LocalBinding"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "signature"),
      Core.fieldTypeType = _TypeSignature_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _ValueBinding_type_}]}))

newtype LocalBindings = 
  LocalBindings {
    unLocalBindings :: [LocalBinding]}
  deriving (Eq, Ord, Read, Show)

_LocalBindings = (Core.Name "hydra/langs/haskell/ast.LocalBindings")

_LocalBindings_type_ = (Core.TypeList _LocalBinding_type_)

data Module = 
  Module {
    moduleHead :: (Maybe ModuleHead),
    moduleImports :: [Import],
    moduleDeclarations :: [DeclarationWithComments]}
  deriving (Eq, Ord, Read, Show)

_Module = (Core.Name "hydra/langs/haskell/ast.Module")

_Module_head = (Core.Name "head")

_Module_imports = (Core.Name "imports")

_Module_declarations = (Core.Name "declarations")

_Module_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Module"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "head"),
      Core.fieldTypeType = (Core.TypeOptional _ModuleHead_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "imports"),
      Core.fieldTypeType = (Core.TypeList _Import_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "declarations"),
      Core.fieldTypeType = (Core.TypeList _DeclarationWithComments_type_)}]}))

data ModuleHead = 
  ModuleHead {
    moduleHeadComments :: (Maybe String),
    moduleHeadName :: ModuleName,
    moduleHeadExports :: [Export]}
  deriving (Eq, Ord, Read, Show)

_ModuleHead = (Core.Name "hydra/langs/haskell/ast.ModuleHead")

_ModuleHead_comments = (Core.Name "comments")

_ModuleHead_name = (Core.Name "name")

_ModuleHead_exports = (Core.Name "exports")

_ModuleHead_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.ModuleHead"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "comments"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _ModuleName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "exports"),
      Core.fieldTypeType = (Core.TypeList _Export_type_)}]}))

newtype ModuleName = 
  ModuleName {
    unModuleName :: String}
  deriving (Eq, Ord, Read, Show)

_ModuleName = (Core.Name "hydra/langs/haskell/ast.ModuleName")

_ModuleName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data Name = 
  NameImplicit QualifiedName |
  NameNormal QualifiedName |
  NameParens QualifiedName
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra/langs/haskell/ast.Name")

_Name_implicit = (Core.Name "implicit")

_Name_normal = (Core.Name "normal")

_Name_parens = (Core.Name "parens")

_Name_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Name"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implicit"),
      Core.fieldTypeType = _QualifiedName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "normal"),
      Core.fieldTypeType = _QualifiedName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parens"),
      Core.fieldTypeType = _QualifiedName_type_}]}))

newtype NamePart = 
  NamePart {
    unNamePart :: String}
  deriving (Eq, Ord, Read, Show)

_NamePart = (Core.Name "hydra/langs/haskell/ast.NamePart")

_NamePart_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data Operator = 
  OperatorBacktick QualifiedName |
  OperatorNormal QualifiedName
  deriving (Eq, Ord, Read, Show)

_Operator = (Core.Name "hydra/langs/haskell/ast.Operator")

_Operator_backtick = (Core.Name "backtick")

_Operator_normal = (Core.Name "normal")

_Operator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Operator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "backtick"),
      Core.fieldTypeType = _QualifiedName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "normal"),
      Core.fieldTypeType = _QualifiedName_type_}]}))

data Pattern = 
  PatternApplication Pattern_Application |
  PatternAs Pattern_As |
  PatternList [Pattern] |
  PatternLiteral Literal |
  PatternName Name |
  PatternParens Pattern |
  PatternRecord Pattern_Record |
  PatternTuple [Pattern] |
  PatternTyped Pattern_Typed |
  PatternWildcard 
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/langs/haskell/ast.Pattern")

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

_Pattern_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Pattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "application"),
      Core.fieldTypeType = _Pattern_Application_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "as"),
      Core.fieldTypeType = _Pattern_As_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = (Core.TypeList _Pattern_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _Literal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parens"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "record"),
      Core.fieldTypeType = _Pattern_Record_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tuple"),
      Core.fieldTypeType = (Core.TypeList _Pattern_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typed"),
      Core.fieldTypeType = _Pattern_Typed_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "wildcard"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data Pattern_Application = 
  Pattern_Application {
    pattern_ApplicationName :: Name,
    pattern_ApplicationArgs :: [Pattern]}
  deriving (Eq, Ord, Read, Show)

_Pattern_Application = (Core.Name "hydra/langs/haskell/ast.Pattern.Application")

_Pattern_Application_name = (Core.Name "name")

_Pattern_Application_args = (Core.Name "args")

_Pattern_Application_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Pattern.Application"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "args"),
      Core.fieldTypeType = (Core.TypeList _Pattern_type_)}]}))

data Pattern_As = 
  Pattern_As {
    pattern_AsName :: Name,
    pattern_AsInner :: Pattern}
  deriving (Eq, Ord, Read, Show)

_Pattern_As = (Core.Name "hydra/langs/haskell/ast.Pattern.As")

_Pattern_As_name = (Core.Name "name")

_Pattern_As_inner = (Core.Name "inner")

_Pattern_As_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Pattern.As"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inner"),
      Core.fieldTypeType = _Pattern_type_}]}))

data Pattern_Record = 
  Pattern_Record {
    pattern_RecordName :: Name,
    pattern_RecordFields :: [PatternField]}
  deriving (Eq, Ord, Read, Show)

_Pattern_Record = (Core.Name "hydra/langs/haskell/ast.Pattern.Record")

_Pattern_Record_name = (Core.Name "name")

_Pattern_Record_fields = (Core.Name "fields")

_Pattern_Record_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Pattern.Record"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fields"),
      Core.fieldTypeType = (Core.TypeList _PatternField_type_)}]}))

data Pattern_Typed = 
  Pattern_Typed {
    pattern_TypedInner :: Pattern,
    pattern_TypedType :: Type}
  deriving (Eq, Ord, Read, Show)

_Pattern_Typed = (Core.Name "hydra/langs/haskell/ast.Pattern.Typed")

_Pattern_Typed_inner = (Core.Name "inner")

_Pattern_Typed_type = (Core.Name "type")

_Pattern_Typed_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Pattern.Typed"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inner"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _Type_type_}]}))

data PatternField = 
  PatternField {
    patternFieldName :: Name,
    patternFieldPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_PatternField = (Core.Name "hydra/langs/haskell/ast.PatternField")

_PatternField_name = (Core.Name "name")

_PatternField_pattern = (Core.Name "pattern")

_PatternField_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.PatternField"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _Pattern_type_}]}))

data QualifiedName = 
  QualifiedName {
    qualifiedNameQualifiers :: [NamePart],
    qualifiedNameUnqualified :: NamePart}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra/langs/haskell/ast.QualifiedName")

_QualifiedName_qualifiers = (Core.Name "qualifiers")

_QualifiedName_unqualified = (Core.Name "unqualified")

_QualifiedName_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.QualifiedName"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifiers"),
      Core.fieldTypeType = (Core.TypeList _NamePart_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unqualified"),
      Core.fieldTypeType = _NamePart_type_}]}))

newtype RightHandSide = 
  RightHandSide {
    unRightHandSide :: Expression}
  deriving (Eq, Ord, Read, Show)

_RightHandSide = (Core.Name "hydra/langs/haskell/ast.RightHandSide")

_RightHandSide_type_ = _Expression_type_

newtype Statement = 
  Statement {
    unStatement :: Expression}
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra/langs/haskell/ast.Statement")

_Statement_type_ = _Expression_type_

data Type = 
  TypeApplication Type_Application |
  TypeCtx Type_Context |
  TypeFunction Type_Function |
  TypeInfix Type_Infix |
  TypeList Type |
  TypeParens Type |
  TypeTuple [Type] |
  TypeVariable Name
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra/langs/haskell/ast.Type")

_Type_application = (Core.Name "application")

_Type_ctx = (Core.Name "ctx")

_Type_function = (Core.Name "function")

_Type_infix = (Core.Name "infix")

_Type_list = (Core.Name "list")

_Type_parens = (Core.Name "parens")

_Type_tuple = (Core.Name "tuple")

_Type_variable = (Core.Name "variable")

_Type_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Type"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "application"),
      Core.fieldTypeType = _Type_Application_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ctx"),
      Core.fieldTypeType = _Type_Context_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "function"),
      Core.fieldTypeType = _Type_Function_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "infix"),
      Core.fieldTypeType = _Type_Infix_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = _Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parens"),
      Core.fieldTypeType = _Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tuple"),
      Core.fieldTypeType = (Core.TypeList _Type_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Name_type_}]}))

data Type_Application = 
  Type_Application {
    type_ApplicationContext :: Type,
    type_ApplicationArgument :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Application = (Core.Name "hydra/langs/haskell/ast.Type.Application")

_Type_Application_context = (Core.Name "context")

_Type_Application_argument = (Core.Name "argument")

_Type_Application_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Type.Application"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "context"),
      Core.fieldTypeType = _Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "argument"),
      Core.fieldTypeType = _Type_type_}]}))

data Type_Context = 
  Type_Context {
    type_ContextCtx :: Assertion,
    type_ContextType :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Context = (Core.Name "hydra/langs/haskell/ast.Type.Context")

_Type_Context_ctx = (Core.Name "ctx")

_Type_Context_type = (Core.Name "type")

_Type_Context_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Type.Context"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ctx"),
      Core.fieldTypeType = _Assertion_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _Type_type_}]}))

data Type_Function = 
  Type_Function {
    type_FunctionDomain :: Type,
    type_FunctionCodomain :: Type}
  deriving (Eq, Ord, Read, Show)

_Type_Function = (Core.Name "hydra/langs/haskell/ast.Type.Function")

_Type_Function_domain = (Core.Name "domain")

_Type_Function_codomain = (Core.Name "codomain")

_Type_Function_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Type.Function"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "domain"),
      Core.fieldTypeType = _Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "codomain"),
      Core.fieldTypeType = _Type_type_}]}))

data Type_Infix = 
  Type_Infix {
    type_InfixLhs :: Type,
    type_InfixOperator :: Operator,
    type_InfixRhs :: Operator}
  deriving (Eq, Ord, Read, Show)

_Type_Infix = (Core.Name "hydra/langs/haskell/ast.Type.Infix")

_Type_Infix_lhs = (Core.Name "lhs")

_Type_Infix_operator = (Core.Name "operator")

_Type_Infix_rhs = (Core.Name "rhs")

_Type_Infix_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.Type.Infix"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _Operator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _Operator_type_}]}))

data TypeDeclaration = 
  TypeDeclaration {
    typeDeclarationName :: DeclarationHead,
    typeDeclarationType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeDeclaration = (Core.Name "hydra/langs/haskell/ast.TypeDeclaration")

_TypeDeclaration_name = (Core.Name "name")

_TypeDeclaration_type = (Core.Name "type")

_TypeDeclaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.TypeDeclaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _DeclarationHead_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _Type_type_}]}))

data TypeSignature = 
  TypeSignature {
    typeSignatureName :: Name,
    typeSignatureType :: Type}
  deriving (Eq, Ord, Read, Show)

_TypeSignature = (Core.Name "hydra/langs/haskell/ast.TypeSignature")

_TypeSignature_name = (Core.Name "name")

_TypeSignature_type = (Core.Name "type")

_TypeSignature_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.TypeSignature"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _Type_type_}]}))

data TypedBinding = 
  TypedBinding {
    typedBindingTypeSignature :: TypeSignature,
    typedBindingValueBinding :: ValueBinding}
  deriving (Eq, Ord, Read, Show)

_TypedBinding = (Core.Name "hydra/langs/haskell/ast.TypedBinding")

_TypedBinding_typeSignature = (Core.Name "typeSignature")

_TypedBinding_valueBinding = (Core.Name "valueBinding")

_TypedBinding_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.TypedBinding"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeSignature"),
      Core.fieldTypeType = _TypeSignature_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "valueBinding"),
      Core.fieldTypeType = _ValueBinding_type_}]}))

data ValueBinding = 
  ValueBindingSimple ValueBinding_Simple
  deriving (Eq, Ord, Read, Show)

_ValueBinding = (Core.Name "hydra/langs/haskell/ast.ValueBinding")

_ValueBinding_simple = (Core.Name "simple")

_ValueBinding_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.ValueBinding"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "simple"),
      Core.fieldTypeType = _ValueBinding_Simple_type_}]}))

data ValueBinding_Simple = 
  ValueBinding_Simple {
    valueBinding_SimplePattern :: Pattern,
    valueBinding_SimpleRhs :: RightHandSide,
    valueBinding_SimpleLocalBindings :: (Maybe LocalBindings)}
  deriving (Eq, Ord, Read, Show)

_ValueBinding_Simple = (Core.Name "hydra/langs/haskell/ast.ValueBinding.Simple")

_ValueBinding_Simple_pattern = (Core.Name "pattern")

_ValueBinding_Simple_rhs = (Core.Name "rhs")

_ValueBinding_Simple_localBindings = (Core.Name "localBindings")

_ValueBinding_Simple_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/haskell/ast.ValueBinding.Simple"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _RightHandSide_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "localBindings"),
      Core.fieldTypeType = (Core.TypeOptional _LocalBindings_type_)}]}))

newtype Variable = 
  Variable {
    unVariable :: Name}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra/langs/haskell/ast.Variable")

_Variable_type_ = _Name_type_