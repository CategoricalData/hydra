{-# LANGUAGE DeriveGeneric #-}
module Hydra.Ext.Haskell.Ast
  ( Alternative(..)
  , Assertion(..)
  , CaseRhs
  , Constructor_Record(..)
  , Constructor(..)
  , DataDeclaration_Keyword(..)
  , DataDeclaration(..)
  , Declaration(..)
  , DeclarationHead_Application(..)
  , DeclarationHead(..)
  , DeclarationWithComments(..)
  , Deriving
  , Export(..)
  , Expression_Application(..)
  , Expression_Case(..)
  , Expression_ConstructRecord(..)
  , Expression_If(..)
  , Expression_InfixApplication(..)
  , Expression_Lambda(..)
  , Expression_Let(..)
  , Expression_PrefixApplication(..)
  , Expression_Section(..)
  , Expression_TypeSignature(..)
  , Expression_UpdateRecord(..)
  , Expression(..)
  , Field(..)
  , FieldUpdate(..)
  , Import_Spec(..)
  , Import(..)
  , ImportExportSpec_SubSpec(..)
  , ImportExportSpec(..)
  , ImportModifier(..)
  , Literal(..)
  , LocalBinding(..)
  , LocalBindings
  , Module(..)
  , ModuleHead(..)
  , ModuleName
  , Name(..)
  , NamePart
  , Operator(..)
  , Pattern_Application(..)
  , Pattern_As(..)
  , Pattern_Record(..)
  , Pattern_Typed(..)
  , Pattern(..)
  , PatternField(..)
  , QualifiedName(..)
  , RightHandSide
  , Statement
  , Type_Application(..)
  , Type_Function(..)
  , Type_Infix(..)
  , Type(..)
  , TypeDeclaration(..)
  , TypeSignature(..)
  , TypeVariable
  , TypedBinding(..)
  , ValueBinding_Simple(..)
  , ValueBinding(..)
  , _Alternative
  , _Alternative_binds
  , _Alternative_pattern
  , _Alternative_rhs
  , _Assertion
  , _Assertion_name
  , _Assertion_types
  , _CaseRhs
  , _Constructor
  , _Constructor_Record
  , _Constructor_Record_fields
  , _Constructor_Record_name
  , _Constructor_record
  , _DataDeclaration
  , _DataDeclaration_Keyword
  , _DataDeclaration_Keyword_data
  , _DataDeclaration_Keyword_newtype
  , _DataDeclaration_constructors
  , _DataDeclaration_context
  , _DataDeclaration_deriving
  , _DataDeclaration_head
  , _DataDeclaration_keyword
  , _Declaration
  , _DeclarationHead
  , _DeclarationHead_Application
  , _DeclarationHead_Application_function
  , _DeclarationHead_Application_operand
  , _DeclarationHead_application
  , _DeclarationHead_parens
  , _DeclarationHead_simple
  , _DeclarationWithComments
  , _DeclarationWithComments_body
  , _DeclarationWithComments_comments
  , _Declaration_data
  , _Declaration_type
  , _Declaration_typedBinding
  , _Deriving
  , _Export
  , _Export_declaration
  , _Export_module
  , _Expression
  , _Expression_Application
  , _Expression_Application_argument
  , _Expression_Application_function
  , _Expression_Case
  , _Expression_Case_alternatives
  , _Expression_Case_case
  , _Expression_ConstructRecord
  , _Expression_ConstructRecord_fields
  , _Expression_ConstructRecord_name
  , _Expression_If
  , _Expression_If_condition
  , _Expression_If_else
  , _Expression_If_then
  , _Expression_InfixApplication
  , _Expression_InfixApplication_lhs
  , _Expression_InfixApplication_operator
  , _Expression_InfixApplication_rhs
  , _Expression_Lambda
  , _Expression_Lambda_bindings
  , _Expression_Lambda_inner
  , _Expression_Let
  , _Expression_Let_bindings
  , _Expression_Let_inner
  , _Expression_PrefixApplication
  , _Expression_PrefixApplication_operator
  , _Expression_PrefixApplication_rhs
  , _Expression_Section
  , _Expression_Section_expression
  , _Expression_Section_operator
  , _Expression_TypeSignature
  , _Expression_TypeSignature_inner
  , _Expression_TypeSignature_type
  , _Expression_UpdateRecord
  , _Expression_UpdateRecord_fields
  , _Expression_UpdateRecord_inner
  , _Expression_application
  , _Expression_case
  , _Expression_constructRecord
  , _Expression_do
  , _Expression_if
  , _Expression_infixApplication
  , _Expression_lambda
  , _Expression_leftSection
  , _Expression_let
  , _Expression_list
  , _Expression_literal
  , _Expression_parens
  , _Expression_prefixApplication
  , _Expression_rightSection
  , _Expression_tuple
  , _Expression_typeSignature
  , _Expression_updateRecord
  , _Expression_variable
  , _Field
  , _FieldUpdate
  , _FieldUpdate_name
  , _FieldUpdate_value
  , _Field_name
  , _Field_type
  , _Import
  , _ImportExportSpec
  , _ImportExportSpec_SubSpec
  , _ImportExportSpec_SubSpec_all
  , _ImportExportSpec_SubSpec_list
  , _ImportExportSpec_modifier
  , _ImportExportSpec_name
  , _ImportExportSpec_subspec
  , _ImportModifier
  , _ImportModifier_pattern
  , _ImportModifier_type
  , _Import_Spec
  , _Import_Spec_hiding
  , _Import_Spec_list
  , _Import_as
  , _Import_module
  , _Import_qualified
  , _Import_spec
  , _Literal
  , _Literal_char
  , _Literal_double
  , _Literal_float
  , _Literal_int
  , _Literal_integer
  , _Literal_string
  , _LocalBinding
  , _LocalBinding_signature
  , _LocalBinding_value
  , _LocalBindings
  , _Module
  , _ModuleHead
  , _ModuleHead_exports
  , _ModuleHead_name
  , _ModuleName
  , _Module_declarations
  , _Module_head
  , _Module_imports
  , _Name
  , _NamePart
  , _Name_implicit
  , _Name_normal
  , _Name_parens
  , _Operator
  , _Operator_backtick
  , _Operator_normal
  , _Pattern
  , _PatternField
  , _PatternField_name
  , _PatternField_pattern
  , _Pattern_Application
  , _Pattern_Application_args
  , _Pattern_Application_name
  , _Pattern_As
  , _Pattern_As_inner
  , _Pattern_As_name
  , _Pattern_Record
  , _Pattern_Record_fields
  , _Pattern_Record_name
  , _Pattern_Typed
  , _Pattern_Typed_inner
  , _Pattern_Typed_type
  , _Pattern_application
  , _Pattern_as
  , _Pattern_list
  , _Pattern_literal
  , _Pattern_name
  , _Pattern_parens
  , _Pattern_record
  , _Pattern_tuple
  , _Pattern_typed
  , _Pattern_wildcard
  , _QualifiedName
  , _QualifiedName_qualifiers
  , _QualifiedName_unqualified
  , _RightHandSide
  , _Statement
  , _Type
  , _TypeDeclaration
  , _TypeDeclaration_head
  , _TypeDeclaration_type
  , _TypeSignature
  , _TypeSignature_name
  , _TypeSignature_type
  , _TypeVariable
  , _Type_Application
  , _Type_Application_argument
  , _Type_Application_context
  , _Type_Function
  , _Type_Function_codomain
  , _Type_Function_domain
  , _Type_Infix
  , _Type_Infix_lhs
  , _Type_Infix_operator
  , _Type_Infix_rhs
  , _Type_application
  , _Type_function
  , _Type_infix
  , _Type_list
  , _Type_parens
  , _Type_tuple
  , _Type_variable
  , _TypedBinding
  , _TypedBinding_typeSignature
  , _TypedBinding_valueBinding
  , _ValueBinding
  , _ValueBinding_Simple
  , _ValueBinding_Simple_localBindings
  , _ValueBinding_Simple_pattern
  , _ValueBinding_Simple_rhs
  , _ValueBinding_simple
  ) where

import GHC.Generics (Generic)
import Data.Int
import Data.Map
import Data.Set

data Alternative
  = Alternative
    -- | @type hydra/ext/haskell/ast.Pattern
    { alternativePattern :: Pattern
    -- | @type hydra/ext/haskell/ast.CaseRhs
    , alternativeRhs :: CaseRhs
    -- | @type optional: hydra/ext/haskell/ast.LocalBindings
    , alternativeBinds :: Maybe LocalBindings } deriving (Eq, Generic, Ord, Read, Show)

data Assertion
  = Assertion
    -- | @type hydra/ext/haskell/ast.Name
    { assertionName :: Name
    -- | @type list: hydra/ext/haskell/ast.Type
    , assertionTypes :: [Type] } deriving (Eq, Generic, Ord, Read, Show)

-- | @type hydra/ext/haskell/ast.Expression
type CaseRhs = Expression

data Constructor_Record
  = Constructor_Record
    -- | @type hydra/ext/haskell/ast.Name
    { constructorRecordName :: Name
    -- | @type list: hydra/ext/haskell/ast.Field
    , constructorRecordFields :: [Field] } deriving (Eq, Generic, Ord, Read, Show)

data Constructor
  -- | @type hydra/ext/haskell/ast.Constructor.Record
  = ConstructorRecord Constructor_Record deriving (Eq, Generic, Ord, Read, Show)

data DataDeclaration_Keyword
  = DataDeclaration_KeywordData
  | DataDeclaration_KeywordNewtype deriving (Eq, Generic, Ord, Read, Show)

data DataDeclaration
  = DataDeclaration
    -- | @type hydra/ext/haskell/ast.DataDeclaration.Keyword
    { dataDeclarationKeyword :: DataDeclaration_Keyword
    -- | @type list: hydra/ext/haskell/ast.Assertion
    , dataDeclarationContext :: [Assertion]
    -- | @type hydra/ext/haskell/ast.DeclarationHead
    , dataDeclarationHead :: DeclarationHead
    -- | @type list: hydra/ext/haskell/ast.Constructor
    , dataDeclarationConstructors :: [Constructor]
    -- | @type list: hydra/ext/haskell/ast.Deriving
    , dataDeclarationDeriving :: [Deriving] } deriving (Eq, Generic, Ord, Read, Show)

data Declaration
  -- | @type hydra/ext/haskell/ast.DataDeclaration
  = DeclarationData DataDeclaration
  -- | @type hydra/ext/haskell/ast.TypeDeclaration
  | DeclarationType TypeDeclaration
  -- | @type hydra/ext/haskell/ast.TypedBinding
  | DeclarationTypedBinding TypedBinding deriving (Eq, Generic, Ord, Read, Show)

data DeclarationHead_Application
  = DeclarationHead_Application
    -- | @type hydra/ext/haskell/ast.DeclarationHead
    { declarationHeadApplicationFunction :: DeclarationHead
    -- | @type hydra/ext/haskell/ast.TypeVariable
    , declarationHeadApplicationOperand :: TypeVariable } deriving (Eq, Generic, Ord, Read, Show)

data DeclarationHead
  -- | @type hydra/ext/haskell/ast.DeclarationHead.Application
  = DeclarationHeadApplication DeclarationHead_Application
  -- | @type hydra/ext/haskell/ast.DeclarationHead
  | DeclarationHeadParens DeclarationHead
  -- | @type hydra/ext/haskell/ast.Name
  | DeclarationHeadSimple Name deriving (Eq, Generic, Ord, Read, Show)

data DeclarationWithComments
  = DeclarationWithComments
    -- | @type hydra/ext/haskell/ast.Declaration
    { declarationWithCommentsBody :: Declaration
    -- | @type optional: string
    , declarationWithCommentsComments :: Maybe String } deriving (Eq, Generic, Ord, Read, Show)

-- | @type list: hydra/ext/haskell/ast.Name
type Deriving = [Name]

data Export
  -- | @type hydra/ext/haskell/ast.ImportExportSpec
  = ExportDeclaration ImportExportSpec
  -- | @type hydra/ext/haskell/ast.ModuleName
  | ExportModule ModuleName deriving (Eq, Generic, Ord, Read, Show)

data Expression_Application
  = Expression_Application
    -- | @type hydra/ext/haskell/ast.Expression
    { expressionApplicationFunction :: Expression
    -- | @type hydra/ext/haskell/ast.Expression
    , expressionApplicationArgument :: Expression } deriving (Eq, Generic, Ord, Read, Show)

data Expression_Case
  = Expression_Case
    -- | @type hydra/ext/haskell/ast.Expression
    { expressionCaseCase :: Expression
    -- | @type list: hydra/ext/haskell/ast.Alternative
    , expressionCaseAlternatives :: [Alternative] } deriving (Eq, Generic, Ord, Read, Show)

data Expression_ConstructRecord
  = Expression_ConstructRecord
    -- | @type hydra/ext/haskell/ast.Name
    { expressionConstructRecordName :: Name
    -- | @type list: hydra/ext/haskell/ast.FieldUpdate
    , expressionConstructRecordFields :: [FieldUpdate] } deriving (Eq, Generic, Ord, Read, Show)

data Expression_If
  = Expression_If
    -- | @type hydra/ext/haskell/ast.Expression
    { expressionIfCondition :: Expression
    -- | @type hydra/ext/haskell/ast.Expression
    , expressionIfThen :: Expression
    -- | @type hydra/ext/haskell/ast.Expression
    , expressionIfElse :: Expression } deriving (Eq, Generic, Ord, Read, Show)

data Expression_InfixApplication
  = Expression_InfixApplication
    -- | @type hydra/ext/haskell/ast.Expression
    { expressionInfixApplicationLhs :: Expression
    -- | @type hydra/ext/haskell/ast.Operator
    , expressionInfixApplicationOperator :: Operator
    -- | @type hydra/ext/haskell/ast.Expression
    , expressionInfixApplicationRhs :: Expression } deriving (Eq, Generic, Ord, Read, Show)

data Expression_Lambda
  = Expression_Lambda
    -- | @type list: hydra/ext/haskell/ast.Pattern
    { expressionLambdaBindings :: [Pattern]
    -- | @type hydra/ext/haskell/ast.Expression
    , expressionLambdaInner :: Expression } deriving (Eq, Generic, Ord, Read, Show)

data Expression_Let
  = Expression_Let
    -- | @type list: hydra/ext/haskell/ast.Pattern
    { expressionLetBindings :: [Pattern]
    -- | @type hydra/ext/haskell/ast.Expression
    , expressionLetInner :: Expression } deriving (Eq, Generic, Ord, Read, Show)

data Expression_PrefixApplication
  = Expression_PrefixApplication
    -- | @type hydra/ext/haskell/ast.Operator
    { expressionPrefixApplicationOperator :: Operator
    -- | @type hydra/ext/haskell/ast.Expression
    , expressionPrefixApplicationRhs :: Expression } deriving (Eq, Generic, Ord, Read, Show)

data Expression_Section
  = Expression_Section
    -- | @type hydra/ext/haskell/ast.Operator
    { expressionSectionOperator :: Operator
    -- | @type hydra/ext/haskell/ast.Expression
    , expressionSectionExpression :: Expression } deriving (Eq, Generic, Ord, Read, Show)

data Expression_TypeSignature
  = Expression_TypeSignature
    -- | @type hydra/ext/haskell/ast.Expression
    { expressionTypeSignatureInner :: Expression
    -- | @type hydra/ext/haskell/ast.Type
    , expressionTypeSignatureType :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Expression_UpdateRecord
  = Expression_UpdateRecord
    -- | @type hydra/ext/haskell/ast.Expression
    { expressionUpdateRecordInner :: Expression
    -- | @type list: hydra/ext/haskell/ast.FieldUpdate
    , expressionUpdateRecordFields :: [FieldUpdate] } deriving (Eq, Generic, Ord, Read, Show)

data Expression
  -- | @type hydra/ext/haskell/ast.Expression.Application
  = ExpressionApplication Expression_Application
  -- | @type hydra/ext/haskell/ast.Expression.Case
  | ExpressionCase Expression_Case
  -- | @type hydra/ext/haskell/ast.Expression.ConstructRecord
  | ExpressionConstructRecord Expression_ConstructRecord
  -- | @type list: hydra/ext/haskell/ast.Statement
  | ExpressionDo [Statement]
  -- | @type hydra/ext/haskell/ast.Expression.If
  | ExpressionIf Expression_If
  -- | @type hydra/ext/haskell/ast.Expression.InfixApplication
  | ExpressionInfixApplication Expression_InfixApplication
  -- | @type hydra/ext/haskell/ast.Literal
  | ExpressionLiteral Literal
  -- | @type hydra/ext/haskell/ast.Expression.Lambda
  | ExpressionLambda Expression_Lambda
  -- | @type hydra/ext/haskell/ast.Expression.Section
  | ExpressionLeftSection Expression_Section
  -- | @type hydra/ext/haskell/ast.Expression.Let
  | ExpressionLet Expression_Let
  -- | @type list: hydra/ext/haskell/ast.Expression
  | ExpressionList [Expression]
  -- | @type hydra/ext/haskell/ast.Expression
  | ExpressionParens Expression
  -- | @type hydra/ext/haskell/ast.Expression.PrefixApplication
  | ExpressionPrefixApplication Expression_PrefixApplication
  -- | @type hydra/ext/haskell/ast.Expression.Section
  | ExpressionRightSection Expression_Section
  -- | @type list: hydra/ext/haskell/ast.Expression
  | ExpressionTuple [Expression]
  -- | @type hydra/ext/haskell/ast.Expression.TypeSignature
  | ExpressionTypeSignature Expression_TypeSignature
  -- | @type hydra/ext/haskell/ast.Expression.UpdateRecord
  | ExpressionUpdateRecord Expression_UpdateRecord
  -- | @type hydra/ext/haskell/ast.Name
  | ExpressionVariable Name deriving (Eq, Generic, Ord, Read, Show)

data Field
  = Field
    -- | @type hydra/ext/haskell/ast.Name
    { fieldName :: Name
    -- | @type hydra/ext/haskell/ast.Type
    , fieldType :: Type } deriving (Eq, Generic, Ord, Read, Show)

data FieldUpdate
  = FieldUpdate
    -- | @type hydra/ext/haskell/ast.Name
    { fieldUpdateName :: Name
    -- | @type hydra/ext/haskell/ast.Expression
    , fieldUpdateValue :: Expression } deriving (Eq, Generic, Ord, Read, Show)

data Import_Spec
  -- | @type list: hydra/ext/haskell/ast.ImportExportSpec
  = Import_SpecList [ImportExportSpec]
  -- | @type list: hydra/ext/haskell/ast.ImportExportSpec
  | Import_SpecHiding [ImportExportSpec] deriving (Eq, Generic, Ord, Read, Show)

data Import
  = Import
    -- | @type boolean
    { importQualified :: Bool
    -- | @type hydra/ext/haskell/ast.ModuleName
    , importModule :: ModuleName
    -- | @type optional: hydra/ext/haskell/ast.ModuleName
    , importAs :: Maybe ModuleName
    -- | @type optional: hydra/ext/haskell/ast.Import.Spec
    , importSpec :: Maybe Import_Spec } deriving (Eq, Generic, Ord, Read, Show)

data ImportExportSpec_SubSpec
  = ImportExportSpec_SubSpecAll
  -- | @type list: hydra/ext/haskell/ast.Name
  | ImportExportSpec_SubSpecList [Name] deriving (Eq, Generic, Ord, Read, Show)

data ImportExportSpec
  = ImportExportSpec
    -- | @type optional: hydra/ext/haskell/ast.ImportModifier
    { importExportSpecModifier :: Maybe ImportModifier
    -- | @type hydra/ext/haskell/ast.Name
    , importExportSpecName :: Name
    -- | @type optional: hydra/ext/haskell/ast.ImportExportSpec.SubSpec
    , importExportSpecSubspec :: Maybe ImportExportSpec_SubSpec } deriving (Eq, Generic, Ord, Read, Show)

data ImportModifier
  = ImportModifierPattern
  | ImportModifierType deriving (Eq, Generic, Ord, Read, Show)

data Literal
  {-| @type integer:
              precision:
                bits: 16
              signed: false -}
  = LiteralChar Integer
  {-| @type float:
              precision:
                bits: 64 -}
  | LiteralDouble Double
  -- | @type float
  | LiteralFloat Float
  -- | @type integer
  | LiteralInt Int
  {-| @type integer:
              precision: arbitrary -}
  | LiteralInteger Integer
  -- | @type string
  | LiteralString String deriving (Eq, Generic, Ord, Read, Show)

data LocalBinding
  -- | @type hydra/ext/haskell/ast.TypeSignature
  = LocalBindingSignature TypeSignature
  -- | @type hydra/ext/haskell/ast.ValueBinding
  | LocalBindingValue ValueBinding deriving (Eq, Generic, Ord, Read, Show)

-- | @type list: hydra/ext/haskell/ast.LocalBinding
type LocalBindings = [LocalBinding]

data Module
  = Module
    -- | @type optional: hydra/ext/haskell/ast.ModuleHead
    { moduleHead :: Maybe ModuleHead
    -- | @type list: hydra/ext/haskell/ast.Import
    , moduleImports :: [Import]
    -- | @type list: hydra/ext/haskell/ast.DeclarationWithComments
    , moduleDeclarations :: [DeclarationWithComments] } deriving (Eq, Generic, Ord, Read, Show)

data ModuleHead
  = ModuleHead
    -- | @type hydra/ext/haskell/ast.ModuleName
    { moduleHeadName :: ModuleName
    -- | @type list: hydra/ext/haskell/ast.Export
    , moduleHeadExports :: [Export] } deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type ModuleName = String

data Name
  -- | @type hydra/ext/haskell/ast.QualifiedName
  = NameImplicit QualifiedName
  -- | @type hydra/ext/haskell/ast.QualifiedName
  | NameNormal QualifiedName
  -- | @type hydra/ext/haskell/ast.QualifiedName
  | NameParens QualifiedName deriving (Eq, Generic, Ord, Read, Show)

-- | @type string
type NamePart = String

data Operator
  -- | @type hydra/ext/haskell/ast.QualifiedName
  = OperatorBacktick QualifiedName
  -- | @type hydra/ext/haskell/ast.QualifiedName
  | OperatorNormal QualifiedName deriving (Eq, Generic, Ord, Read, Show)

data Pattern_Application
  = Pattern_Application
    -- | @type hydra/ext/haskell/ast.Name
    { patternApplicationName :: Name
    -- | @type list: hydra/ext/haskell/ast.Pattern
    , patternApplicationArgs :: [Pattern] } deriving (Eq, Generic, Ord, Read, Show)

data Pattern_As
  = Pattern_As
    -- | @type hydra/ext/haskell/ast.Name
    { patternAsName :: Name
    -- | @type hydra/ext/haskell/ast.Pattern
    , patternAsInner :: Pattern } deriving (Eq, Generic, Ord, Read, Show)

data Pattern_Record
  = Pattern_Record
    -- | @type hydra/ext/haskell/ast.Name
    { patternRecordName :: Name
    -- | @type list: hydra/ext/haskell/ast.PatternField
    , patternRecordFields :: [PatternField] } deriving (Eq, Generic, Ord, Read, Show)

data Pattern_Typed
  = Pattern_Typed
    -- | @type hydra/ext/haskell/ast.Pattern
    { patternTypedInner :: Pattern
    -- | @type hydra/ext/haskell/ast.Type
    , patternTypedType :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Pattern
  -- | @type hydra/ext/haskell/ast.Pattern.Application
  = PatternApplication Pattern_Application
  -- | @type hydra/ext/haskell/ast.Pattern.As
  | PatternAs Pattern_As
  -- | @type list: hydra/ext/haskell/ast.Pattern
  | PatternList [Pattern]
  -- | @type hydra/ext/haskell/ast.Literal
  | PatternLiteral Literal
  -- | @type hydra/ext/haskell/ast.Name
  | PatternName Name
  -- | @type hydra/ext/haskell/ast.Pattern
  | PatternParens Pattern
  -- | @type hydra/ext/haskell/ast.Pattern.Record
  | PatternRecord Pattern_Record
  -- | @type list: hydra/ext/haskell/ast.Pattern
  | PatternTuple [Pattern]
  -- | @type hydra/ext/haskell/ast.Pattern.Typed
  | PatternTyped Pattern_Typed
  | PatternWildcard deriving (Eq, Generic, Ord, Read, Show)

data PatternField
  = PatternField
    -- | @type hydra/ext/haskell/ast.Name
    { patternFieldName :: Name
    -- | @type hydra/ext/haskell/ast.Pattern
    , patternFieldPattern :: Pattern } deriving (Eq, Generic, Ord, Read, Show)

data QualifiedName
  = QualifiedName
    -- | @type list: hydra/ext/haskell/ast.NamePart
    { qualifiedNameQualifiers :: [NamePart]
    -- | @type hydra/ext/haskell/ast.NamePart
    , qualifiedNameUnqualified :: NamePart } deriving (Eq, Generic, Ord, Read, Show)

-- | @type hydra/ext/haskell/ast.Expression
type RightHandSide = Expression

-- | @type hydra/ext/haskell/ast.Expression
type Statement = Expression

data Type_Application
  = Type_Application
    -- | @type hydra/ext/haskell/ast.Type
    { typeApplicationContext :: Type
    -- | @type hydra/ext/haskell/ast.Type
    , typeApplicationArgument :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_Function
  = Type_Function
    -- | @type hydra/ext/haskell/ast.Type
    { typeFunctionDomain :: Type
    -- | @type hydra/ext/haskell/ast.Type
    , typeFunctionCodomain :: Type } deriving (Eq, Generic, Ord, Read, Show)

data Type_Infix
  = Type_Infix
    -- | @type hydra/ext/haskell/ast.Type
    { typeInfixLhs :: Type
    -- | @type hydra/ext/haskell/ast.Operator
    , typeInfixOperator :: Operator
    -- | @type hydra/ext/haskell/ast.Operator
    , typeInfixRhs :: Operator } deriving (Eq, Generic, Ord, Read, Show)

data Type
  -- | @type hydra/ext/haskell/ast.Type.Application
  = TypeApplication Type_Application
  -- | @type hydra/ext/haskell/ast.Type.Function
  | TypeFunction Type_Function
  -- | @type hydra/ext/haskell/ast.Type.Infix
  | TypeInfix Type_Infix
  -- | @type hydra/ext/haskell/ast.Type
  | TypeList Type
  -- | @type hydra/ext/haskell/ast.Type
  | TypeParens Type
  -- | @type list: hydra/ext/haskell/ast.Type
  | TypeTuple [Type]
  -- | @type hydra/ext/haskell/ast.Name
  | TypeVariable Name deriving (Eq, Generic, Ord, Read, Show)

data TypeDeclaration
  = TypeDeclaration
    -- | @type hydra/ext/haskell/ast.DeclarationHead
    { typeDeclarationHead :: DeclarationHead
    -- | @type hydra/ext/haskell/ast.Type
    , typeDeclarationType :: Type } deriving (Eq, Generic, Ord, Read, Show)

data TypeSignature
  = TypeSignature
    -- | @type hydra/ext/haskell/ast.Name
    { typeSignatureName :: Name
    -- | @type hydra/ext/haskell/ast.Type
    , typeSignatureType :: Type } deriving (Eq, Generic, Ord, Read, Show)

-- | @type hydra/ext/haskell/ast.Name
type TypeVariable = Name

data TypedBinding
  = TypedBinding
    -- | @type hydra/ext/haskell/ast.TypeSignature
    { typedBindingTypeSignature :: TypeSignature
    -- | @type hydra/ext/haskell/ast.ValueBinding
    , typedBindingValueBinding :: ValueBinding } deriving (Eq, Generic, Ord, Read, Show)

data ValueBinding_Simple
  = ValueBinding_Simple
    -- | @type hydra/ext/haskell/ast.Pattern
    { valueBindingSimplePattern :: Pattern
    -- | @type hydra/ext/haskell/ast.RightHandSide
    , valueBindingSimpleRhs :: RightHandSide
    -- | @type optional: hydra/ext/haskell/ast.LocalBindings
    , valueBindingSimpleLocalBindings :: Maybe LocalBindings } deriving (Eq, Generic, Ord, Read, Show)

data ValueBinding
  -- | @type hydra/ext/haskell/ast.ValueBinding.Simple
  = ValueBindingSimple ValueBinding_Simple deriving (Eq, Generic, Ord, Read, Show)

_Alternative = "hydra/ext/haskell/ast.Alternative" :: String
_Alternative_binds = "binds" :: String
_Alternative_pattern = "pattern" :: String
_Alternative_rhs = "rhs" :: String
_Assertion = "hydra/ext/haskell/ast.Assertion" :: String
_Assertion_name = "name" :: String
_Assertion_types = "types" :: String
_CaseRhs = "hydra/ext/haskell/ast.CaseRhs" :: String
_Constructor = "hydra/ext/haskell/ast.Constructor" :: String
_Constructor_Record = "hydra/ext/haskell/ast.Constructor_Record" :: String
_Constructor_Record_fields = "fields" :: String
_Constructor_Record_name = "name" :: String
_Constructor_record = "record" :: String
_DataDeclaration = "hydra/ext/haskell/ast.DataDeclaration" :: String
_DataDeclaration_Keyword = "hydra/ext/haskell/ast.DataDeclaration_Keyword" :: String
_DataDeclaration_Keyword_data = "data" :: String
_DataDeclaration_Keyword_newtype = "newtype" :: String
_DataDeclaration_constructors = "constructors" :: String
_DataDeclaration_context = "context" :: String
_DataDeclaration_deriving = "deriving" :: String
_DataDeclaration_head = "head" :: String
_DataDeclaration_keyword = "keyword" :: String
_Declaration = "hydra/ext/haskell/ast.Declaration" :: String
_DeclarationHead = "hydra/ext/haskell/ast.DeclarationHead" :: String
_DeclarationHead_Application = "hydra/ext/haskell/ast.DeclarationHead_Application" :: String
_DeclarationHead_Application_function = "function" :: String
_DeclarationHead_Application_operand = "operand" :: String
_DeclarationHead_application = "application" :: String
_DeclarationHead_parens = "parens" :: String
_DeclarationHead_simple = "simple" :: String
_DeclarationWithComments = "hydra/ext/haskell/ast.DeclarationWithComments" :: String
_DeclarationWithComments_body = "body" :: String
_DeclarationWithComments_comments = "comments" :: String
_Declaration_data = "data" :: String
_Declaration_type = "type" :: String
_Declaration_typedBinding = "typedBinding" :: String
_Deriving = "hydra/ext/haskell/ast.Deriving" :: String
_Export = "hydra/ext/haskell/ast.Export" :: String
_Export_declaration = "declaration" :: String
_Export_module = "module" :: String
_Expression = "hydra/ext/haskell/ast.Expression" :: String
_Expression_Application = "hydra/ext/haskell/ast.Expression_Application" :: String
_Expression_Application_argument = "argument" :: String
_Expression_Application_function = "function" :: String
_Expression_Case = "hydra/ext/haskell/ast.Expression_Case" :: String
_Expression_Case_alternatives = "alternatives" :: String
_Expression_Case_case = "case" :: String
_Expression_ConstructRecord = "hydra/ext/haskell/ast.Expression_ConstructRecord" :: String
_Expression_ConstructRecord_fields = "fields" :: String
_Expression_ConstructRecord_name = "name" :: String
_Expression_If = "hydra/ext/haskell/ast.Expression_If" :: String
_Expression_If_condition = "condition" :: String
_Expression_If_else = "else" :: String
_Expression_If_then = "then" :: String
_Expression_InfixApplication = "hydra/ext/haskell/ast.Expression_InfixApplication" :: String
_Expression_InfixApplication_lhs = "lhs" :: String
_Expression_InfixApplication_operator = "operator" :: String
_Expression_InfixApplication_rhs = "rhs" :: String
_Expression_Lambda = "hydra/ext/haskell/ast.Expression_Lambda" :: String
_Expression_Lambda_bindings = "bindings" :: String
_Expression_Lambda_inner = "inner" :: String
_Expression_Let = "hydra/ext/haskell/ast.Expression_Let" :: String
_Expression_Let_bindings = "bindings" :: String
_Expression_Let_inner = "inner" :: String
_Expression_PrefixApplication = "hydra/ext/haskell/ast.Expression_PrefixApplication" :: String
_Expression_PrefixApplication_operator = "operator" :: String
_Expression_PrefixApplication_rhs = "rhs" :: String
_Expression_Section = "hydra/ext/haskell/ast.Expression_Section" :: String
_Expression_Section_expression = "expression" :: String
_Expression_Section_operator = "operator" :: String
_Expression_TypeSignature = "hydra/ext/haskell/ast.Expression_TypeSignature" :: String
_Expression_TypeSignature_inner = "inner" :: String
_Expression_TypeSignature_type = "type" :: String
_Expression_UpdateRecord = "hydra/ext/haskell/ast.Expression_UpdateRecord" :: String
_Expression_UpdateRecord_fields = "fields" :: String
_Expression_UpdateRecord_inner = "inner" :: String
_Expression_application = "application" :: String
_Expression_case = "case" :: String
_Expression_constructRecord = "constructRecord" :: String
_Expression_do = "do" :: String
_Expression_if = "if" :: String
_Expression_infixApplication = "infixApplication" :: String
_Expression_lambda = "lambda" :: String
_Expression_leftSection = "leftSection" :: String
_Expression_let = "let" :: String
_Expression_list = "list" :: String
_Expression_literal = "literal" :: String
_Expression_parens = "parens" :: String
_Expression_prefixApplication = "prefixApplication" :: String
_Expression_rightSection = "rightSection" :: String
_Expression_tuple = "tuple" :: String
_Expression_typeSignature = "typeSignature" :: String
_Expression_updateRecord = "updateRecord" :: String
_Expression_variable = "variable" :: String
_Field = "hydra/ext/haskell/ast.Field" :: String
_FieldUpdate = "hydra/ext/haskell/ast.FieldUpdate" :: String
_FieldUpdate_name = "name" :: String
_FieldUpdate_value = "value" :: String
_Field_name = "name" :: String
_Field_type = "type" :: String
_Import = "hydra/ext/haskell/ast.Import" :: String
_ImportExportSpec = "hydra/ext/haskell/ast.ImportExportSpec" :: String
_ImportExportSpec_SubSpec = "hydra/ext/haskell/ast.ImportExportSpec_SubSpec" :: String
_ImportExportSpec_SubSpec_all = "all" :: String
_ImportExportSpec_SubSpec_list = "list" :: String
_ImportExportSpec_modifier = "modifier" :: String
_ImportExportSpec_name = "name" :: String
_ImportExportSpec_subspec = "subspec" :: String
_ImportModifier = "hydra/ext/haskell/ast.ImportModifier" :: String
_ImportModifier_pattern = "pattern" :: String
_ImportModifier_type = "type" :: String
_Import_Spec = "hydra/ext/haskell/ast.Import_Spec" :: String
_Import_Spec_hiding = "hiding" :: String
_Import_Spec_list = "list" :: String
_Import_as = "as" :: String
_Import_module = "module" :: String
_Import_qualified = "qualified" :: String
_Import_spec = "spec" :: String
_Literal = "hydra/ext/haskell/ast.Literal" :: String
_Literal_char = "char" :: String
_Literal_double = "double" :: String
_Literal_float = "float" :: String
_Literal_int = "int" :: String
_Literal_integer = "integer" :: String
_Literal_string = "string" :: String
_LocalBinding = "hydra/ext/haskell/ast.LocalBinding" :: String
_LocalBinding_signature = "signature" :: String
_LocalBinding_value = "value" :: String
_LocalBindings = "hydra/ext/haskell/ast.LocalBindings" :: String
_Module = "hydra/ext/haskell/ast.Module" :: String
_ModuleHead = "hydra/ext/haskell/ast.ModuleHead" :: String
_ModuleHead_exports = "exports" :: String
_ModuleHead_name = "name" :: String
_ModuleName = "hydra/ext/haskell/ast.ModuleName" :: String
_Module_declarations = "declarations" :: String
_Module_head = "head" :: String
_Module_imports = "imports" :: String
_Name = "hydra/ext/haskell/ast.Name" :: String
_NamePart = "hydra/ext/haskell/ast.NamePart" :: String
_Name_implicit = "implicit" :: String
_Name_normal = "normal" :: String
_Name_parens = "parens" :: String
_Operator = "hydra/ext/haskell/ast.Operator" :: String
_Operator_backtick = "backtick" :: String
_Operator_normal = "normal" :: String
_Pattern = "hydra/ext/haskell/ast.Pattern" :: String
_PatternField = "hydra/ext/haskell/ast.PatternField" :: String
_PatternField_name = "name" :: String
_PatternField_pattern = "pattern" :: String
_Pattern_Application = "hydra/ext/haskell/ast.Pattern_Application" :: String
_Pattern_Application_args = "args" :: String
_Pattern_Application_name = "name" :: String
_Pattern_As = "hydra/ext/haskell/ast.Pattern_As" :: String
_Pattern_As_inner = "inner" :: String
_Pattern_As_name = "name" :: String
_Pattern_Record = "hydra/ext/haskell/ast.Pattern_Record" :: String
_Pattern_Record_fields = "fields" :: String
_Pattern_Record_name = "name" :: String
_Pattern_Typed = "hydra/ext/haskell/ast.Pattern_Typed" :: String
_Pattern_Typed_inner = "inner" :: String
_Pattern_Typed_type = "type" :: String
_Pattern_application = "application" :: String
_Pattern_as = "as" :: String
_Pattern_list = "list" :: String
_Pattern_literal = "literal" :: String
_Pattern_name = "name" :: String
_Pattern_parens = "parens" :: String
_Pattern_record = "record" :: String
_Pattern_tuple = "tuple" :: String
_Pattern_typed = "typed" :: String
_Pattern_wildcard = "wildcard" :: String
_QualifiedName = "hydra/ext/haskell/ast.QualifiedName" :: String
_QualifiedName_qualifiers = "qualifiers" :: String
_QualifiedName_unqualified = "unqualified" :: String
_RightHandSide = "hydra/ext/haskell/ast.RightHandSide" :: String
_Statement = "hydra/ext/haskell/ast.Statement" :: String
_Type = "hydra/ext/haskell/ast.Type" :: String
_TypeDeclaration = "hydra/ext/haskell/ast.TypeDeclaration" :: String
_TypeDeclaration_head = "head" :: String
_TypeDeclaration_type = "type" :: String
_TypeSignature = "hydra/ext/haskell/ast.TypeSignature" :: String
_TypeSignature_name = "name" :: String
_TypeSignature_type = "type" :: String
_TypeVariable = "hydra/ext/haskell/ast.TypeVariable" :: String
_Type_Application = "hydra/ext/haskell/ast.Type_Application" :: String
_Type_Application_argument = "argument" :: String
_Type_Application_context = "context" :: String
_Type_Function = "hydra/ext/haskell/ast.Type_Function" :: String
_Type_Function_codomain = "codomain" :: String
_Type_Function_domain = "domain" :: String
_Type_Infix = "hydra/ext/haskell/ast.Type_Infix" :: String
_Type_Infix_lhs = "lhs" :: String
_Type_Infix_operator = "operator" :: String
_Type_Infix_rhs = "rhs" :: String
_Type_application = "application" :: String
_Type_function = "function" :: String
_Type_infix = "infix" :: String
_Type_list = "list" :: String
_Type_parens = "parens" :: String
_Type_tuple = "tuple" :: String
_Type_variable = "variable" :: String
_TypedBinding = "hydra/ext/haskell/ast.TypedBinding" :: String
_TypedBinding_typeSignature = "typeSignature" :: String
_TypedBinding_valueBinding = "valueBinding" :: String
_ValueBinding = "hydra/ext/haskell/ast.ValueBinding" :: String
_ValueBinding_Simple = "hydra/ext/haskell/ast.ValueBinding_Simple" :: String
_ValueBinding_Simple_localBindings = "localBindings" :: String
_ValueBinding_Simple_pattern = "pattern" :: String
_ValueBinding_Simple_rhs = "rhs" :: String
_ValueBinding_simple = "simple" :: String
