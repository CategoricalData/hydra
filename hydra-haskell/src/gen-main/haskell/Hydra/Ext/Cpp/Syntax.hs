-- | A C++ syntax model, focusing on features for representing algebraic data types and declarative computations

module Hydra.Ext.Cpp.Syntax where

import qualified Hydra.Core as Core
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data AccessSpecifier = 
  AccessSpecifierPublic  |
  AccessSpecifierProtected  |
  AccessSpecifierPrivate  |
  AccessSpecifierNone 
  deriving (Eq, Ord, Read, Show)

_AccessSpecifier = (Core.Name "hydra.ext.cpp.syntax.AccessSpecifier")

_AccessSpecifier_public = (Core.Name "public")

_AccessSpecifier_protected = (Core.Name "protected")

_AccessSpecifier_private = (Core.Name "private")

_AccessSpecifier_none = (Core.Name "none")

data Program = 
  Program {
    programPreprocessorDirectives :: [PreprocessorDirective],
    programIncludes :: [IncludeDirective],
    programDeclarations :: [Declaration]}
  deriving (Eq, Ord, Read, Show)

_Program = (Core.Name "hydra.ext.cpp.syntax.Program")

_Program_preprocessorDirectives = (Core.Name "preprocessorDirectives")

_Program_includes = (Core.Name "includes")

_Program_declarations = (Core.Name "declarations")

data IncludeDirective = 
  IncludeDirective {
    includeDirectiveName :: String,
    includeDirectiveIsSystem :: Bool}
  deriving (Eq, Ord, Read, Show)

_IncludeDirective = (Core.Name "hydra.ext.cpp.syntax.IncludeDirective")

_IncludeDirective_name = (Core.Name "name")

_IncludeDirective_isSystem = (Core.Name "isSystem")

data Declaration = 
  DeclarationPreprocessor PreprocessorDirective |
  DeclarationClass ClassDeclaration |
  DeclarationFunction FunctionDeclaration |
  DeclarationVariable VariableDeclaration |
  DeclarationTypedef TypedefDeclaration |
  DeclarationNamespace NamespaceDeclaration |
  DeclarationTemplate TemplateDeclaration
  deriving (Eq, Ord, Read, Show)

_Declaration = (Core.Name "hydra.ext.cpp.syntax.Declaration")

_Declaration_preprocessor = (Core.Name "preprocessor")

_Declaration_class = (Core.Name "class")

_Declaration_function = (Core.Name "function")

_Declaration_variable = (Core.Name "variable")

_Declaration_typedef = (Core.Name "typedef")

_Declaration_namespace = (Core.Name "namespace")

_Declaration_template = (Core.Name "template")

data NamespaceDeclaration = 
  NamespaceDeclaration {
    namespaceDeclarationName :: String,
    namespaceDeclarationDeclarations :: [Declaration]}
  deriving (Eq, Ord, Read, Show)

_NamespaceDeclaration = (Core.Name "hydra.ext.cpp.syntax.NamespaceDeclaration")

_NamespaceDeclaration_name = (Core.Name "name")

_NamespaceDeclaration_declarations = (Core.Name "declarations")

data TypedefDeclaration = 
  TypedefDeclaration {
    typedefDeclarationName :: String,
    typedefDeclarationType :: TypeExpression,
    typedefDeclarationIsUsing :: Bool}
  deriving (Eq, Ord, Read, Show)

_TypedefDeclaration = (Core.Name "hydra.ext.cpp.syntax.TypedefDeclaration")

_TypedefDeclaration_name = (Core.Name "name")

_TypedefDeclaration_type = (Core.Name "type")

_TypedefDeclaration_isUsing = (Core.Name "isUsing")

data ClassDeclaration = 
  ClassDeclaration {
    classDeclarationSpecifier :: ClassSpecifier,
    classDeclarationBody :: (Maybe ClassBody)}
  deriving (Eq, Ord, Read, Show)

_ClassDeclaration = (Core.Name "hydra.ext.cpp.syntax.ClassDeclaration")

_ClassDeclaration_specifier = (Core.Name "specifier")

_ClassDeclaration_body = (Core.Name "body")

data TemplateDeclaration = 
  TemplateDeclaration {
    templateDeclarationInline :: Bool,
    templateDeclarationParameters :: [String],
    templateDeclarationDeclaration :: Declaration}
  deriving (Eq, Ord, Read, Show)

_TemplateDeclaration = (Core.Name "hydra.ext.cpp.syntax.TemplateDeclaration")

_TemplateDeclaration_inline = (Core.Name "inline")

_TemplateDeclaration_parameters = (Core.Name "parameters")

_TemplateDeclaration_declaration = (Core.Name "declaration")

data PreprocessorDirective = 
  PreprocessorDirectiveInclude IncludeDirective |
  PreprocessorDirectivePragma PragmaDirective |
  PreprocessorDirectiveDefine DefineDirective |
  PreprocessorDirectiveUndef UndefDirective |
  PreprocessorDirectiveIfdef IfdefDirective |
  PreprocessorDirectiveIfndef IfndefDirective |
  PreprocessorDirectiveIf IfDirective |
  PreprocessorDirectiveElif ElifDirective |
  PreprocessorDirectiveElse ElseDirective |
  PreprocessorDirectiveEndif EndifDirective |
  PreprocessorDirectiveLine LineDirective |
  PreprocessorDirectiveError ErrorDirective |
  PreprocessorDirectiveWarning WarningDirective
  deriving (Eq, Ord, Read, Show)

_PreprocessorDirective = (Core.Name "hydra.ext.cpp.syntax.PreprocessorDirective")

_PreprocessorDirective_include = (Core.Name "include")

_PreprocessorDirective_pragma = (Core.Name "pragma")

_PreprocessorDirective_define = (Core.Name "define")

_PreprocessorDirective_undef = (Core.Name "undef")

_PreprocessorDirective_ifdef = (Core.Name "ifdef")

_PreprocessorDirective_ifndef = (Core.Name "ifndef")

_PreprocessorDirective_if = (Core.Name "if")

_PreprocessorDirective_elif = (Core.Name "elif")

_PreprocessorDirective_else = (Core.Name "else")

_PreprocessorDirective_endif = (Core.Name "endif")

_PreprocessorDirective_line = (Core.Name "line")

_PreprocessorDirective_error = (Core.Name "error")

_PreprocessorDirective_warning = (Core.Name "warning")

data PragmaDirective = 
  PragmaDirective {
    pragmaDirectiveContent :: String}
  deriving (Eq, Ord, Read, Show)

_PragmaDirective = (Core.Name "hydra.ext.cpp.syntax.PragmaDirective")

_PragmaDirective_content = (Core.Name "content")

data DefineDirective = 
  DefineDirective {
    defineDirectiveName :: String,
    defineDirectiveParameters :: (Maybe [String]),
    defineDirectiveReplacement :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_DefineDirective = (Core.Name "hydra.ext.cpp.syntax.DefineDirective")

_DefineDirective_name = (Core.Name "name")

_DefineDirective_parameters = (Core.Name "parameters")

_DefineDirective_replacement = (Core.Name "replacement")

data UndefDirective = 
  UndefDirective {
    undefDirectiveName :: String}
  deriving (Eq, Ord, Read, Show)

_UndefDirective = (Core.Name "hydra.ext.cpp.syntax.UndefDirective")

_UndefDirective_name = (Core.Name "name")

data IfdefDirective = 
  IfdefDirective {
    ifdefDirectiveIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_IfdefDirective = (Core.Name "hydra.ext.cpp.syntax.IfdefDirective")

_IfdefDirective_identifier = (Core.Name "identifier")

data IfndefDirective = 
  IfndefDirective {
    ifndefDirectiveIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_IfndefDirective = (Core.Name "hydra.ext.cpp.syntax.IfndefDirective")

_IfndefDirective_identifier = (Core.Name "identifier")

data IfDirective = 
  IfDirective {
    ifDirectiveCondition :: String}
  deriving (Eq, Ord, Read, Show)

_IfDirective = (Core.Name "hydra.ext.cpp.syntax.IfDirective")

_IfDirective_condition = (Core.Name "condition")

data ElifDirective = 
  ElifDirective {
    elifDirectiveCondition :: String}
  deriving (Eq, Ord, Read, Show)

_ElifDirective = (Core.Name "hydra.ext.cpp.syntax.ElifDirective")

_ElifDirective_condition = (Core.Name "condition")

data ElseDirective = 
  ElseDirective {}
  deriving (Eq, Ord, Read, Show)

_ElseDirective = (Core.Name "hydra.ext.cpp.syntax.ElseDirective")

data EndifDirective = 
  EndifDirective {}
  deriving (Eq, Ord, Read, Show)

_EndifDirective = (Core.Name "hydra.ext.cpp.syntax.EndifDirective")

data LineDirective = 
  LineDirective {
    lineDirectiveLineNumber :: Int,
    lineDirectiveFilename :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_LineDirective = (Core.Name "hydra.ext.cpp.syntax.LineDirective")

_LineDirective_lineNumber = (Core.Name "lineNumber")

_LineDirective_filename = (Core.Name "filename")

data ErrorDirective = 
  ErrorDirective {
    errorDirectiveMessage :: String}
  deriving (Eq, Ord, Read, Show)

_ErrorDirective = (Core.Name "hydra.ext.cpp.syntax.ErrorDirective")

_ErrorDirective_message = (Core.Name "message")

data WarningDirective = 
  WarningDirective {
    warningDirectiveMessage :: String}
  deriving (Eq, Ord, Read, Show)

_WarningDirective = (Core.Name "hydra.ext.cpp.syntax.WarningDirective")

_WarningDirective_message = (Core.Name "message")

data ClassSpecifier = 
  ClassSpecifier {
    classSpecifierKey :: ClassKey,
    classSpecifierName :: String,
    classSpecifierInheritance :: [BaseSpecifier]}
  deriving (Eq, Ord, Read, Show)

_ClassSpecifier = (Core.Name "hydra.ext.cpp.syntax.ClassSpecifier")

_ClassSpecifier_key = (Core.Name "key")

_ClassSpecifier_name = (Core.Name "name")

_ClassSpecifier_inheritance = (Core.Name "inheritance")

data ClassKey = 
  ClassKeyClass  |
  ClassKeyEnumClass  |
  ClassKeyStruct 
  deriving (Eq, Ord, Read, Show)

_ClassKey = (Core.Name "hydra.ext.cpp.syntax.ClassKey")

_ClassKey_class = (Core.Name "class")

_ClassKey_enumClass = (Core.Name "enumClass")

_ClassKey_struct = (Core.Name "struct")

data BaseSpecifier = 
  BaseSpecifier {
    baseSpecifierAccess :: AccessSpecifier,
    baseSpecifierName :: String}
  deriving (Eq, Ord, Read, Show)

_BaseSpecifier = (Core.Name "hydra.ext.cpp.syntax.BaseSpecifier")

_BaseSpecifier_access = (Core.Name "access")

_BaseSpecifier_name = (Core.Name "name")

newtype ClassBody = 
  ClassBody {
    unClassBody :: [MemberSpecification]}
  deriving (Eq, Ord, Read, Show)

_ClassBody = (Core.Name "hydra.ext.cpp.syntax.ClassBody")

data MemberSpecification = 
  MemberSpecificationAccessLabel AccessSpecifier |
  MemberSpecificationMember MemberDeclaration
  deriving (Eq, Ord, Read, Show)

_MemberSpecification = (Core.Name "hydra.ext.cpp.syntax.MemberSpecification")

_MemberSpecification_accessLabel = (Core.Name "accessLabel")

_MemberSpecification_member = (Core.Name "member")

data MemberDeclaration = 
  MemberDeclarationFunction FunctionDeclaration |
  MemberDeclarationVariable VariableDeclaration |
  MemberDeclarationConstructor ConstructorDeclaration |
  MemberDeclarationDestructor DestructorDeclaration |
  MemberDeclarationNestedClass ClassDeclaration |
  MemberDeclarationTemplate TemplateDeclaration
  deriving (Eq, Ord, Read, Show)

_MemberDeclaration = (Core.Name "hydra.ext.cpp.syntax.MemberDeclaration")

_MemberDeclaration_function = (Core.Name "function")

_MemberDeclaration_variable = (Core.Name "variable")

_MemberDeclaration_constructor = (Core.Name "constructor")

_MemberDeclaration_destructor = (Core.Name "destructor")

_MemberDeclaration_nestedClass = (Core.Name "nestedClass")

_MemberDeclaration_template = (Core.Name "template")

data ConstructorDeclaration = 
  ConstructorDeclaration {
    constructorDeclarationName :: String,
    constructorDeclarationParameters :: [Parameter],
    constructorDeclarationInitializers :: [MemInitializer],
    constructorDeclarationBody :: FunctionBody}
  deriving (Eq, Ord, Read, Show)

_ConstructorDeclaration = (Core.Name "hydra.ext.cpp.syntax.ConstructorDeclaration")

_ConstructorDeclaration_name = (Core.Name "name")

_ConstructorDeclaration_parameters = (Core.Name "parameters")

_ConstructorDeclaration_initializers = (Core.Name "initializers")

_ConstructorDeclaration_body = (Core.Name "body")

data MemInitializer = 
  MemInitializer {
    memInitializerName :: String,
    memInitializerArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_MemInitializer = (Core.Name "hydra.ext.cpp.syntax.MemInitializer")

_MemInitializer_name = (Core.Name "name")

_MemInitializer_arguments = (Core.Name "arguments")

data DestructorDeclaration = 
  DestructorDeclaration {
    destructorDeclarationPrefixSpecifiers :: [FunctionSpecifierPrefix],
    destructorDeclarationName :: String,
    destructorDeclarationSuffixSpecifiers :: [FunctionSpecifierSuffix],
    destructorDeclarationBody :: FunctionBody}
  deriving (Eq, Ord, Read, Show)

_DestructorDeclaration = (Core.Name "hydra.ext.cpp.syntax.DestructorDeclaration")

_DestructorDeclaration_prefixSpecifiers = (Core.Name "prefixSpecifiers")

_DestructorDeclaration_name = (Core.Name "name")

_DestructorDeclaration_suffixSpecifiers = (Core.Name "suffixSpecifiers")

_DestructorDeclaration_body = (Core.Name "body")

data FunctionDeclaration = 
  FunctionDeclaration {
    functionDeclarationPrefixSpecifiers :: [FunctionSpecifierPrefix],
    functionDeclarationReturnType :: TypeExpression,
    functionDeclarationName :: String,
    functionDeclarationParameters :: [Parameter],
    functionDeclarationSuffixSpecifiers :: [FunctionSpecifierSuffix],
    functionDeclarationBody :: FunctionBody}
  deriving (Eq, Ord, Read, Show)

_FunctionDeclaration = (Core.Name "hydra.ext.cpp.syntax.FunctionDeclaration")

_FunctionDeclaration_prefixSpecifiers = (Core.Name "prefixSpecifiers")

_FunctionDeclaration_returnType = (Core.Name "returnType")

_FunctionDeclaration_name = (Core.Name "name")

_FunctionDeclaration_parameters = (Core.Name "parameters")

_FunctionDeclaration_suffixSpecifiers = (Core.Name "suffixSpecifiers")

_FunctionDeclaration_body = (Core.Name "body")

data FunctionSpecifierPrefix = 
  FunctionSpecifierPrefixVirtual  |
  FunctionSpecifierPrefixStatic  |
  FunctionSpecifierPrefixExplicit 
  deriving (Eq, Ord, Read, Show)

_FunctionSpecifierPrefix = (Core.Name "hydra.ext.cpp.syntax.FunctionSpecifierPrefix")

_FunctionSpecifierPrefix_virtual = (Core.Name "virtual")

_FunctionSpecifierPrefix_static = (Core.Name "static")

_FunctionSpecifierPrefix_explicit = (Core.Name "explicit")

data FunctionSpecifierSuffix = 
  FunctionSpecifierSuffixConst  |
  FunctionSpecifierSuffixNoexcept  |
  FunctionSpecifierSuffixOverride  |
  FunctionSpecifierSuffixFinal 
  deriving (Eq, Ord, Read, Show)

_FunctionSpecifierSuffix = (Core.Name "hydra.ext.cpp.syntax.FunctionSpecifierSuffix")

_FunctionSpecifierSuffix_const = (Core.Name "const")

_FunctionSpecifierSuffix_noexcept = (Core.Name "noexcept")

_FunctionSpecifierSuffix_override = (Core.Name "override")

_FunctionSpecifierSuffix_final = (Core.Name "final")

data Parameter = 
  Parameter {
    parameterType :: TypeExpression,
    parameterName :: String,
    parameterDefaultValue :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_Parameter = (Core.Name "hydra.ext.cpp.syntax.Parameter")

_Parameter_type = (Core.Name "type")

_Parameter_name = (Core.Name "name")

_Parameter_defaultValue = (Core.Name "defaultValue")

data FunctionBody = 
  FunctionBodyCompound CompoundStatement |
  FunctionBodyDeclaration  |
  FunctionBodyPure  |
  FunctionBodyDefault 
  deriving (Eq, Ord, Read, Show)

_FunctionBody = (Core.Name "hydra.ext.cpp.syntax.FunctionBody")

_FunctionBody_compound = (Core.Name "compound")

_FunctionBody_declaration = (Core.Name "declaration")

_FunctionBody_pure = (Core.Name "pure")

_FunctionBody_default = (Core.Name "default")

data VariableDeclaration = 
  VariableDeclaration {
    variableDeclarationType :: (Maybe TypeExpression),
    variableDeclarationName :: String,
    variableDeclarationInitializer :: (Maybe Expression),
    variableDeclarationIsAuto :: Bool}
  deriving (Eq, Ord, Read, Show)

_VariableDeclaration = (Core.Name "hydra.ext.cpp.syntax.VariableDeclaration")

_VariableDeclaration_type = (Core.Name "type")

_VariableDeclaration_name = (Core.Name "name")

_VariableDeclaration_initializer = (Core.Name "initializer")

_VariableDeclaration_isAuto = (Core.Name "isAuto")

data VariantDeclaration = 
  VariantDeclaration {
    variantDeclarationTypes :: [TypeExpression],
    variantDeclarationName :: String}
  deriving (Eq, Ord, Read, Show)

_VariantDeclaration = (Core.Name "hydra.ext.cpp.syntax.VariantDeclaration")

_VariantDeclaration_types = (Core.Name "types")

_VariantDeclaration_name = (Core.Name "name")

data ProductDeclaration = 
  ProductDeclaration {
    productDeclarationName :: String,
    productDeclarationFields :: [VariableDeclaration]}
  deriving (Eq, Ord, Read, Show)

_ProductDeclaration = (Core.Name "hydra.ext.cpp.syntax.ProductDeclaration")

_ProductDeclaration_name = (Core.Name "name")

_ProductDeclaration_fields = (Core.Name "fields")

data ContainerDeclaration = 
  ContainerDeclarationList ListDeclaration |
  ContainerDeclarationMap MapDeclaration |
  ContainerDeclarationSet SetDeclaration |
  ContainerDeclarationOptional OptionalDeclaration
  deriving (Eq, Ord, Read, Show)

_ContainerDeclaration = (Core.Name "hydra.ext.cpp.syntax.ContainerDeclaration")

_ContainerDeclaration_list = (Core.Name "list")

_ContainerDeclaration_map = (Core.Name "map")

_ContainerDeclaration_set = (Core.Name "set")

_ContainerDeclaration_optional = (Core.Name "optional")

data ListDeclaration = 
  ListDeclaration {
    listDeclarationElementType :: TypeExpression,
    listDeclarationName :: String}
  deriving (Eq, Ord, Read, Show)

_ListDeclaration = (Core.Name "hydra.ext.cpp.syntax.ListDeclaration")

_ListDeclaration_elementType = (Core.Name "elementType")

_ListDeclaration_name = (Core.Name "name")

data MapDeclaration = 
  MapDeclaration {
    mapDeclarationKeyType :: TypeExpression,
    mapDeclarationValueType :: TypeExpression,
    mapDeclarationName :: String}
  deriving (Eq, Ord, Read, Show)

_MapDeclaration = (Core.Name "hydra.ext.cpp.syntax.MapDeclaration")

_MapDeclaration_keyType = (Core.Name "keyType")

_MapDeclaration_valueType = (Core.Name "valueType")

_MapDeclaration_name = (Core.Name "name")

data SetDeclaration = 
  SetDeclaration {
    setDeclarationElementType :: TypeExpression,
    setDeclarationName :: String}
  deriving (Eq, Ord, Read, Show)

_SetDeclaration = (Core.Name "hydra.ext.cpp.syntax.SetDeclaration")

_SetDeclaration_elementType = (Core.Name "elementType")

_SetDeclaration_name = (Core.Name "name")

data OptionalDeclaration = 
  OptionalDeclaration {
    optionalDeclarationValueType :: TypeExpression,
    optionalDeclarationName :: String}
  deriving (Eq, Ord, Read, Show)

_OptionalDeclaration = (Core.Name "hydra.ext.cpp.syntax.OptionalDeclaration")

_OptionalDeclaration_valueType = (Core.Name "valueType")

_OptionalDeclaration_name = (Core.Name "name")

data Expression = 
  ExpressionAssignment AssignmentExpression |
  ExpressionComma CommaExpression
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra.ext.cpp.syntax.Expression")

_Expression_assignment = (Core.Name "assignment")

_Expression_comma = (Core.Name "comma")

data CommaExpression = 
  CommaExpression {
    commaExpressionLeft :: Expression,
    commaExpressionRight :: AssignmentExpression}
  deriving (Eq, Ord, Read, Show)

_CommaExpression = (Core.Name "hydra.ext.cpp.syntax.CommaExpression")

_CommaExpression_left = (Core.Name "left")

_CommaExpression_right = (Core.Name "right")

data AssignmentExpression = 
  AssignmentExpressionConditional ConditionalExpression |
  AssignmentExpressionAssignment ExplicitAssignment
  deriving (Eq, Ord, Read, Show)

_AssignmentExpression = (Core.Name "hydra.ext.cpp.syntax.AssignmentExpression")

_AssignmentExpression_conditional = (Core.Name "conditional")

_AssignmentExpression_assignment = (Core.Name "assignment")

data ExplicitAssignment = 
  ExplicitAssignment {
    explicitAssignmentLeft :: LogicalOrExpression,
    explicitAssignmentOp :: AssignmentOperator,
    explicitAssignmentRight :: AssignmentExpression}
  deriving (Eq, Ord, Read, Show)

_ExplicitAssignment = (Core.Name "hydra.ext.cpp.syntax.ExplicitAssignment")

_ExplicitAssignment_left = (Core.Name "left")

_ExplicitAssignment_op = (Core.Name "op")

_ExplicitAssignment_right = (Core.Name "right")

data AssignmentOperator = 
  AssignmentOperatorAssign  |
  AssignmentOperatorPlusAssign  |
  AssignmentOperatorMinusAssign  |
  AssignmentOperatorMultiplyAssign  |
  AssignmentOperatorDivideAssign  |
  AssignmentOperatorModuloAssign  |
  AssignmentOperatorLeftShiftAssign  |
  AssignmentOperatorRightShiftAssign  |
  AssignmentOperatorBitwiseAndAssign  |
  AssignmentOperatorBitwiseXorAssign  |
  AssignmentOperatorBitwiseOrAssign 
  deriving (Eq, Ord, Read, Show)

_AssignmentOperator = (Core.Name "hydra.ext.cpp.syntax.AssignmentOperator")

_AssignmentOperator_assign = (Core.Name "assign")

_AssignmentOperator_plusAssign = (Core.Name "plusAssign")

_AssignmentOperator_minusAssign = (Core.Name "minusAssign")

_AssignmentOperator_multiplyAssign = (Core.Name "multiplyAssign")

_AssignmentOperator_divideAssign = (Core.Name "divideAssign")

_AssignmentOperator_moduloAssign = (Core.Name "moduloAssign")

_AssignmentOperator_leftShiftAssign = (Core.Name "leftShiftAssign")

_AssignmentOperator_rightShiftAssign = (Core.Name "rightShiftAssign")

_AssignmentOperator_bitwiseAndAssign = (Core.Name "bitwiseAndAssign")

_AssignmentOperator_bitwiseXorAssign = (Core.Name "bitwiseXorAssign")

_AssignmentOperator_bitwiseOrAssign = (Core.Name "bitwiseOrAssign")

data ConditionalExpression = 
  ConditionalExpressionLogicalOr LogicalOrExpression |
  ConditionalExpressionTernary TernaryExpression
  deriving (Eq, Ord, Read, Show)

_ConditionalExpression = (Core.Name "hydra.ext.cpp.syntax.ConditionalExpression")

_ConditionalExpression_logicalOr = (Core.Name "logicalOr")

_ConditionalExpression_ternary = (Core.Name "ternary")

data TernaryExpression = 
  TernaryExpression {
    ternaryExpressionCondition :: LogicalOrExpression,
    ternaryExpressionTrueExpr :: Expression,
    ternaryExpressionFalseExpr :: ConditionalExpression}
  deriving (Eq, Ord, Read, Show)

_TernaryExpression = (Core.Name "hydra.ext.cpp.syntax.TernaryExpression")

_TernaryExpression_condition = (Core.Name "condition")

_TernaryExpression_trueExpr = (Core.Name "trueExpr")

_TernaryExpression_falseExpr = (Core.Name "falseExpr")

data LogicalOrExpression = 
  LogicalOrExpressionLogicalAnd LogicalAndExpression |
  LogicalOrExpressionLogicalOr LogicalOrOperation
  deriving (Eq, Ord, Read, Show)

_LogicalOrExpression = (Core.Name "hydra.ext.cpp.syntax.LogicalOrExpression")

_LogicalOrExpression_logicalAnd = (Core.Name "logicalAnd")

_LogicalOrExpression_logicalOr = (Core.Name "logicalOr")

data LogicalOrOperation = 
  LogicalOrOperation {
    logicalOrOperationLeft :: LogicalOrExpression,
    logicalOrOperationRight :: LogicalAndExpression}
  deriving (Eq, Ord, Read, Show)

_LogicalOrOperation = (Core.Name "hydra.ext.cpp.syntax.LogicalOrOperation")

_LogicalOrOperation_left = (Core.Name "left")

_LogicalOrOperation_right = (Core.Name "right")

data LogicalAndExpression = 
  LogicalAndExpressionInclusiveOr InclusiveOrExpression |
  LogicalAndExpressionLogicalAnd LogicalAndOperation
  deriving (Eq, Ord, Read, Show)

_LogicalAndExpression = (Core.Name "hydra.ext.cpp.syntax.LogicalAndExpression")

_LogicalAndExpression_inclusiveOr = (Core.Name "inclusiveOr")

_LogicalAndExpression_logicalAnd = (Core.Name "logicalAnd")

data LogicalAndOperation = 
  LogicalAndOperation {
    logicalAndOperationLeft :: LogicalAndExpression,
    logicalAndOperationRight :: InclusiveOrExpression}
  deriving (Eq, Ord, Read, Show)

_LogicalAndOperation = (Core.Name "hydra.ext.cpp.syntax.LogicalAndOperation")

_LogicalAndOperation_left = (Core.Name "left")

_LogicalAndOperation_right = (Core.Name "right")

data InclusiveOrExpression = 
  InclusiveOrExpressionExclusiveOr ExclusiveOrExpression |
  InclusiveOrExpressionBitwiseOr BitwiseOrOperation
  deriving (Eq, Ord, Read, Show)

_InclusiveOrExpression = (Core.Name "hydra.ext.cpp.syntax.InclusiveOrExpression")

_InclusiveOrExpression_exclusiveOr = (Core.Name "exclusiveOr")

_InclusiveOrExpression_bitwiseOr = (Core.Name "bitwiseOr")

data BitwiseOrOperation = 
  BitwiseOrOperation {
    bitwiseOrOperationLeft :: InclusiveOrExpression,
    bitwiseOrOperationRight :: ExclusiveOrExpression}
  deriving (Eq, Ord, Read, Show)

_BitwiseOrOperation = (Core.Name "hydra.ext.cpp.syntax.BitwiseOrOperation")

_BitwiseOrOperation_left = (Core.Name "left")

_BitwiseOrOperation_right = (Core.Name "right")

data ExclusiveOrExpression = 
  ExclusiveOrExpressionAnd AndExpression |
  ExclusiveOrExpressionBitwiseXor BitwiseXorOperation
  deriving (Eq, Ord, Read, Show)

_ExclusiveOrExpression = (Core.Name "hydra.ext.cpp.syntax.ExclusiveOrExpression")

_ExclusiveOrExpression_and = (Core.Name "and")

_ExclusiveOrExpression_bitwiseXor = (Core.Name "bitwiseXor")

data BitwiseXorOperation = 
  BitwiseXorOperation {
    bitwiseXorOperationLeft :: ExclusiveOrExpression,
    bitwiseXorOperationRight :: AndExpression}
  deriving (Eq, Ord, Read, Show)

_BitwiseXorOperation = (Core.Name "hydra.ext.cpp.syntax.BitwiseXorOperation")

_BitwiseXorOperation_left = (Core.Name "left")

_BitwiseXorOperation_right = (Core.Name "right")

data AndExpression = 
  AndExpressionEquality EqualityExpression |
  AndExpressionBitwiseAnd BitwiseAndOperation
  deriving (Eq, Ord, Read, Show)

_AndExpression = (Core.Name "hydra.ext.cpp.syntax.AndExpression")

_AndExpression_equality = (Core.Name "equality")

_AndExpression_bitwiseAnd = (Core.Name "bitwiseAnd")

data BitwiseAndOperation = 
  BitwiseAndOperation {
    bitwiseAndOperationLeft :: AndExpression,
    bitwiseAndOperationRight :: EqualityExpression}
  deriving (Eq, Ord, Read, Show)

_BitwiseAndOperation = (Core.Name "hydra.ext.cpp.syntax.BitwiseAndOperation")

_BitwiseAndOperation_left = (Core.Name "left")

_BitwiseAndOperation_right = (Core.Name "right")

data EqualityExpression = 
  EqualityExpressionRelational RelationalExpression |
  EqualityExpressionEqual EqualOperation |
  EqualityExpressionNotEqual NotEqualOperation
  deriving (Eq, Ord, Read, Show)

_EqualityExpression = (Core.Name "hydra.ext.cpp.syntax.EqualityExpression")

_EqualityExpression_relational = (Core.Name "relational")

_EqualityExpression_equal = (Core.Name "equal")

_EqualityExpression_notEqual = (Core.Name "notEqual")

data EqualOperation = 
  EqualOperation {
    equalOperationLeft :: EqualityExpression,
    equalOperationRight :: RelationalExpression}
  deriving (Eq, Ord, Read, Show)

_EqualOperation = (Core.Name "hydra.ext.cpp.syntax.EqualOperation")

_EqualOperation_left = (Core.Name "left")

_EqualOperation_right = (Core.Name "right")

data NotEqualOperation = 
  NotEqualOperation {
    notEqualOperationLeft :: EqualityExpression,
    notEqualOperationRight :: RelationalExpression}
  deriving (Eq, Ord, Read, Show)

_NotEqualOperation = (Core.Name "hydra.ext.cpp.syntax.NotEqualOperation")

_NotEqualOperation_left = (Core.Name "left")

_NotEqualOperation_right = (Core.Name "right")

data RelationalExpression = 
  RelationalExpressionShift ShiftExpression |
  RelationalExpressionLess LessOperation |
  RelationalExpressionGreater GreaterOperation |
  RelationalExpressionLessEqual LessEqualOperation |
  RelationalExpressionGreaterEqual GreaterEqualOperation
  deriving (Eq, Ord, Read, Show)

_RelationalExpression = (Core.Name "hydra.ext.cpp.syntax.RelationalExpression")

_RelationalExpression_shift = (Core.Name "shift")

_RelationalExpression_less = (Core.Name "less")

_RelationalExpression_greater = (Core.Name "greater")

_RelationalExpression_lessEqual = (Core.Name "lessEqual")

_RelationalExpression_greaterEqual = (Core.Name "greaterEqual")

data LessOperation = 
  LessOperation {
    lessOperationLeft :: RelationalExpression,
    lessOperationRight :: ShiftExpression}
  deriving (Eq, Ord, Read, Show)

_LessOperation = (Core.Name "hydra.ext.cpp.syntax.LessOperation")

_LessOperation_left = (Core.Name "left")

_LessOperation_right = (Core.Name "right")

data GreaterOperation = 
  GreaterOperation {
    greaterOperationLeft :: RelationalExpression,
    greaterOperationRight :: ShiftExpression}
  deriving (Eq, Ord, Read, Show)

_GreaterOperation = (Core.Name "hydra.ext.cpp.syntax.GreaterOperation")

_GreaterOperation_left = (Core.Name "left")

_GreaterOperation_right = (Core.Name "right")

data LessEqualOperation = 
  LessEqualOperation {
    lessEqualOperationLeft :: RelationalExpression,
    lessEqualOperationRight :: ShiftExpression}
  deriving (Eq, Ord, Read, Show)

_LessEqualOperation = (Core.Name "hydra.ext.cpp.syntax.LessEqualOperation")

_LessEqualOperation_left = (Core.Name "left")

_LessEqualOperation_right = (Core.Name "right")

data GreaterEqualOperation = 
  GreaterEqualOperation {
    greaterEqualOperationLeft :: RelationalExpression,
    greaterEqualOperationRight :: ShiftExpression}
  deriving (Eq, Ord, Read, Show)

_GreaterEqualOperation = (Core.Name "hydra.ext.cpp.syntax.GreaterEqualOperation")

_GreaterEqualOperation_left = (Core.Name "left")

_GreaterEqualOperation_right = (Core.Name "right")

data ShiftExpression = 
  ShiftExpressionAdditive AdditiveExpression |
  ShiftExpressionLeftShift LeftShiftOperation |
  ShiftExpressionRightShift RightShiftOperation
  deriving (Eq, Ord, Read, Show)

_ShiftExpression = (Core.Name "hydra.ext.cpp.syntax.ShiftExpression")

_ShiftExpression_additive = (Core.Name "additive")

_ShiftExpression_leftShift = (Core.Name "leftShift")

_ShiftExpression_rightShift = (Core.Name "rightShift")

data LeftShiftOperation = 
  LeftShiftOperation {
    leftShiftOperationLeft :: ShiftExpression,
    leftShiftOperationRight :: AdditiveExpression}
  deriving (Eq, Ord, Read, Show)

_LeftShiftOperation = (Core.Name "hydra.ext.cpp.syntax.LeftShiftOperation")

_LeftShiftOperation_left = (Core.Name "left")

_LeftShiftOperation_right = (Core.Name "right")

data RightShiftOperation = 
  RightShiftOperation {
    rightShiftOperationLeft :: ShiftExpression,
    rightShiftOperationRight :: AdditiveExpression}
  deriving (Eq, Ord, Read, Show)

_RightShiftOperation = (Core.Name "hydra.ext.cpp.syntax.RightShiftOperation")

_RightShiftOperation_left = (Core.Name "left")

_RightShiftOperation_right = (Core.Name "right")

data AdditiveExpression = 
  AdditiveExpressionMultiplicative MultiplicativeExpression |
  AdditiveExpressionAdd AddOperation |
  AdditiveExpressionSubtract SubtractOperation
  deriving (Eq, Ord, Read, Show)

_AdditiveExpression = (Core.Name "hydra.ext.cpp.syntax.AdditiveExpression")

_AdditiveExpression_multiplicative = (Core.Name "multiplicative")

_AdditiveExpression_add = (Core.Name "add")

_AdditiveExpression_subtract = (Core.Name "subtract")

data AddOperation = 
  AddOperation {
    addOperationLeft :: AdditiveExpression,
    addOperationRight :: MultiplicativeExpression}
  deriving (Eq, Ord, Read, Show)

_AddOperation = (Core.Name "hydra.ext.cpp.syntax.AddOperation")

_AddOperation_left = (Core.Name "left")

_AddOperation_right = (Core.Name "right")

data SubtractOperation = 
  SubtractOperation {
    subtractOperationLeft :: AdditiveExpression,
    subtractOperationRight :: MultiplicativeExpression}
  deriving (Eq, Ord, Read, Show)

_SubtractOperation = (Core.Name "hydra.ext.cpp.syntax.SubtractOperation")

_SubtractOperation_left = (Core.Name "left")

_SubtractOperation_right = (Core.Name "right")

data MultiplicativeExpression = 
  MultiplicativeExpressionUnary UnaryExpression |
  MultiplicativeExpressionMultiply MultiplyOperation |
  MultiplicativeExpressionDivide DivideOperation |
  MultiplicativeExpressionModulo ModuloOperation
  deriving (Eq, Ord, Read, Show)

_MultiplicativeExpression = (Core.Name "hydra.ext.cpp.syntax.MultiplicativeExpression")

_MultiplicativeExpression_unary = (Core.Name "unary")

_MultiplicativeExpression_multiply = (Core.Name "multiply")

_MultiplicativeExpression_divide = (Core.Name "divide")

_MultiplicativeExpression_modulo = (Core.Name "modulo")

data MultiplyOperation = 
  MultiplyOperation {
    multiplyOperationLeft :: MultiplicativeExpression,
    multiplyOperationRight :: UnaryExpression}
  deriving (Eq, Ord, Read, Show)

_MultiplyOperation = (Core.Name "hydra.ext.cpp.syntax.MultiplyOperation")

_MultiplyOperation_left = (Core.Name "left")

_MultiplyOperation_right = (Core.Name "right")

data DivideOperation = 
  DivideOperation {
    divideOperationLeft :: MultiplicativeExpression,
    divideOperationRight :: UnaryExpression}
  deriving (Eq, Ord, Read, Show)

_DivideOperation = (Core.Name "hydra.ext.cpp.syntax.DivideOperation")

_DivideOperation_left = (Core.Name "left")

_DivideOperation_right = (Core.Name "right")

data ModuloOperation = 
  ModuloOperation {
    moduloOperationLeft :: MultiplicativeExpression,
    moduloOperationRight :: UnaryExpression}
  deriving (Eq, Ord, Read, Show)

_ModuloOperation = (Core.Name "hydra.ext.cpp.syntax.ModuloOperation")

_ModuloOperation_left = (Core.Name "left")

_ModuloOperation_right = (Core.Name "right")

data UnaryExpression = 
  UnaryExpressionPostfix PostfixExpression |
  UnaryExpressionUnaryOp UnaryOperation |
  UnaryExpressionSizeof SizeofExpression
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra.ext.cpp.syntax.UnaryExpression")

_UnaryExpression_postfix = (Core.Name "postfix")

_UnaryExpression_unaryOp = (Core.Name "unaryOp")

_UnaryExpression_sizeof = (Core.Name "sizeof")

data UnaryOperation = 
  UnaryOperation {
    unaryOperationOperator :: UnaryOperator,
    unaryOperationOperand :: UnaryExpression}
  deriving (Eq, Ord, Read, Show)

_UnaryOperation = (Core.Name "hydra.ext.cpp.syntax.UnaryOperation")

_UnaryOperation_operator = (Core.Name "operator")

_UnaryOperation_operand = (Core.Name "operand")

data UnaryOperator = 
  UnaryOperatorPlus  |
  UnaryOperatorMinus  |
  UnaryOperatorLogicalNot  |
  UnaryOperatorBitwiseNot  |
  UnaryOperatorDereference  |
  UnaryOperatorAddressOf  |
  UnaryOperatorPreIncrement  |
  UnaryOperatorPreDecrement 
  deriving (Eq, Ord, Read, Show)

_UnaryOperator = (Core.Name "hydra.ext.cpp.syntax.UnaryOperator")

_UnaryOperator_plus = (Core.Name "plus")

_UnaryOperator_minus = (Core.Name "minus")

_UnaryOperator_logicalNot = (Core.Name "logicalNot")

_UnaryOperator_bitwiseNot = (Core.Name "bitwiseNot")

_UnaryOperator_dereference = (Core.Name "dereference")

_UnaryOperator_addressOf = (Core.Name "addressOf")

_UnaryOperator_preIncrement = (Core.Name "preIncrement")

_UnaryOperator_preDecrement = (Core.Name "preDecrement")

newtype SizeofExpression = 
  SizeofExpression {
    unSizeofExpression :: TypeExpression}
  deriving (Eq, Ord, Read, Show)

_SizeofExpression = (Core.Name "hydra.ext.cpp.syntax.SizeofExpression")

data PostfixExpression = 
  PostfixExpressionPrimary PrimaryExpression |
  PostfixExpressionSubscript SubscriptOperation |
  PostfixExpressionFunctionCall FunctionCallOperation |
  PostfixExpressionTemplateFunctionCall TemplateFunctionCallOperation |
  PostfixExpressionMemberAccess MemberAccessOperation |
  PostfixExpressionPointerMemberAccess PointerMemberAccessOperation |
  PostfixExpressionPostIncrement PostfixExpression |
  PostfixExpressionPostDecrement PostfixExpression
  deriving (Eq, Ord, Read, Show)

_PostfixExpression = (Core.Name "hydra.ext.cpp.syntax.PostfixExpression")

_PostfixExpression_primary = (Core.Name "primary")

_PostfixExpression_subscript = (Core.Name "subscript")

_PostfixExpression_functionCall = (Core.Name "functionCall")

_PostfixExpression_templateFunctionCall = (Core.Name "templateFunctionCall")

_PostfixExpression_memberAccess = (Core.Name "memberAccess")

_PostfixExpression_pointerMemberAccess = (Core.Name "pointerMemberAccess")

_PostfixExpression_postIncrement = (Core.Name "postIncrement")

_PostfixExpression_postDecrement = (Core.Name "postDecrement")

data SubscriptOperation = 
  SubscriptOperation {
    subscriptOperationArray :: PostfixExpression,
    subscriptOperationIndex :: Expression}
  deriving (Eq, Ord, Read, Show)

_SubscriptOperation = (Core.Name "hydra.ext.cpp.syntax.SubscriptOperation")

_SubscriptOperation_array = (Core.Name "array")

_SubscriptOperation_index = (Core.Name "index")

data FunctionCallOperation = 
  FunctionCallOperation {
    functionCallOperationFunction :: PostfixExpression,
    functionCallOperationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_FunctionCallOperation = (Core.Name "hydra.ext.cpp.syntax.FunctionCallOperation")

_FunctionCallOperation_function = (Core.Name "function")

_FunctionCallOperation_arguments = (Core.Name "arguments")

data MemberAccessOperation = 
  MemberAccessOperation {
    memberAccessOperationObject :: PostfixExpression,
    memberAccessOperationMember :: String}
  deriving (Eq, Ord, Read, Show)

_MemberAccessOperation = (Core.Name "hydra.ext.cpp.syntax.MemberAccessOperation")

_MemberAccessOperation_object = (Core.Name "object")

_MemberAccessOperation_member = (Core.Name "member")

data PointerMemberAccessOperation = 
  PointerMemberAccessOperation {
    pointerMemberAccessOperationPointer :: PostfixExpression,
    pointerMemberAccessOperationMember :: String}
  deriving (Eq, Ord, Read, Show)

_PointerMemberAccessOperation = (Core.Name "hydra.ext.cpp.syntax.PointerMemberAccessOperation")

_PointerMemberAccessOperation_pointer = (Core.Name "pointer")

_PointerMemberAccessOperation_member = (Core.Name "member")

data TemplateFunctionCallOperation = 
  TemplateFunctionCallOperation {
    templateFunctionCallOperationFunction :: PostfixExpression,
    templateFunctionCallOperationTemplateArguments :: [TemplateArgument],
    templateFunctionCallOperationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_TemplateFunctionCallOperation = (Core.Name "hydra.ext.cpp.syntax.TemplateFunctionCallOperation")

_TemplateFunctionCallOperation_function = (Core.Name "function")

_TemplateFunctionCallOperation_templateArguments = (Core.Name "templateArguments")

_TemplateFunctionCallOperation_arguments = (Core.Name "arguments")

data PrimaryExpression = 
  PrimaryExpressionIdentifier String |
  PrimaryExpressionLiteral Literal |
  PrimaryExpressionParenthesized Expression |
  PrimaryExpressionLambda LambdaExpression
  deriving (Eq, Ord, Read, Show)

_PrimaryExpression = (Core.Name "hydra.ext.cpp.syntax.PrimaryExpression")

_PrimaryExpression_identifier = (Core.Name "identifier")

_PrimaryExpression_literal = (Core.Name "literal")

_PrimaryExpression_parenthesized = (Core.Name "parenthesized")

_PrimaryExpression_lambda = (Core.Name "lambda")

data LambdaExpression = 
  LambdaExpression {
    lambdaExpressionCaptures :: CaptureList,
    lambdaExpressionParameters :: [Parameter],
    lambdaExpressionReturnType :: (Maybe TypeExpression),
    lambdaExpressionBody :: CompoundStatement}
  deriving (Eq, Ord, Read, Show)

_LambdaExpression = (Core.Name "hydra.ext.cpp.syntax.LambdaExpression")

_LambdaExpression_captures = (Core.Name "captures")

_LambdaExpression_parameters = (Core.Name "parameters")

_LambdaExpression_returnType = (Core.Name "returnType")

_LambdaExpression_body = (Core.Name "body")

data CaptureList = 
  CaptureListCaptureByValue  |
  CaptureListCaptures [Capture]
  deriving (Eq, Ord, Read, Show)

_CaptureList = (Core.Name "hydra.ext.cpp.syntax.CaptureList")

_CaptureList_captureByValue = (Core.Name "captureByValue")

_CaptureList_captures = (Core.Name "captures")

data Capture = 
  Capture {
    captureName :: String,
    captureByReference :: Bool}
  deriving (Eq, Ord, Read, Show)

_Capture = (Core.Name "hydra.ext.cpp.syntax.Capture")

_Capture_name = (Core.Name "name")

_Capture_byReference = (Core.Name "byReference")

data PatternMatch = 
  PatternMatch {
    patternMatchVisitor :: Visitor,
    patternMatchVariant :: Expression}
  deriving (Eq, Ord, Read, Show)

_PatternMatch = (Core.Name "hydra.ext.cpp.syntax.PatternMatch")

_PatternMatch_visitor = (Core.Name "visitor")

_PatternMatch_variant = (Core.Name "variant")

data Visitor = 
  VisitorLambda LambdaExpression |
  VisitorOverloaded OverloadedLambdas
  deriving (Eq, Ord, Read, Show)

_Visitor = (Core.Name "hydra.ext.cpp.syntax.Visitor")

_Visitor_lambda = (Core.Name "lambda")

_Visitor_overloaded = (Core.Name "overloaded")

newtype OverloadedLambdas = 
  OverloadedLambdas {
    unOverloadedLambdas :: [LambdaExpression]}
  deriving (Eq, Ord, Read, Show)

_OverloadedLambdas = (Core.Name "hydra.ext.cpp.syntax.OverloadedLambdas")

data FunctionApplication = 
  FunctionApplication {
    functionApplicationFunction :: FunctionIdentifier,
    functionApplicationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_FunctionApplication = (Core.Name "hydra.ext.cpp.syntax.FunctionApplication")

_FunctionApplication_function = (Core.Name "function")

_FunctionApplication_arguments = (Core.Name "arguments")

data FunctionIdentifier = 
  FunctionIdentifierSimple String |
  FunctionIdentifierQualified QualifiedIdentifier
  deriving (Eq, Ord, Read, Show)

_FunctionIdentifier = (Core.Name "hydra.ext.cpp.syntax.FunctionIdentifier")

_FunctionIdentifier_simple = (Core.Name "simple")

_FunctionIdentifier_qualified = (Core.Name "qualified")

data QualifiedIdentifier = 
  QualifiedIdentifier {
    qualifiedIdentifierNamespace :: String,
    qualifiedIdentifierName :: String}
  deriving (Eq, Ord, Read, Show)

_QualifiedIdentifier = (Core.Name "hydra.ext.cpp.syntax.QualifiedIdentifier")

_QualifiedIdentifier_namespace = (Core.Name "namespace")

_QualifiedIdentifier_name = (Core.Name "name")

data Statement = 
  StatementLabeled LabeledStatement |
  StatementCompound CompoundStatement |
  StatementSelection SelectionStatement |
  StatementSwitch SwitchStatement |
  StatementIteration IterationStatement |
  StatementJump JumpStatement |
  StatementDeclaration VariableDeclaration |
  StatementExpression Expression
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra.ext.cpp.syntax.Statement")

_Statement_labeled = (Core.Name "labeled")

_Statement_compound = (Core.Name "compound")

_Statement_selection = (Core.Name "selection")

_Statement_switch = (Core.Name "switch")

_Statement_iteration = (Core.Name "iteration")

_Statement_jump = (Core.Name "jump")

_Statement_declaration = (Core.Name "declaration")

_Statement_expression = (Core.Name "expression")

data LabeledStatement = 
  LabeledStatement {
    labeledStatementLabel :: String,
    labeledStatementStatement :: Statement}
  deriving (Eq, Ord, Read, Show)

_LabeledStatement = (Core.Name "hydra.ext.cpp.syntax.LabeledStatement")

_LabeledStatement_label = (Core.Name "label")

_LabeledStatement_statement = (Core.Name "statement")

newtype CompoundStatement = 
  CompoundStatement {
    unCompoundStatement :: [Statement]}
  deriving (Eq, Ord, Read, Show)

_CompoundStatement = (Core.Name "hydra.ext.cpp.syntax.CompoundStatement")

data SelectionStatement = 
  SelectionStatement {
    selectionStatementCondition :: Expression,
    selectionStatementThenBranch :: Statement,
    selectionStatementElseBranch :: (Maybe Statement)}
  deriving (Eq, Ord, Read, Show)

_SelectionStatement = (Core.Name "hydra.ext.cpp.syntax.SelectionStatement")

_SelectionStatement_condition = (Core.Name "condition")

_SelectionStatement_thenBranch = (Core.Name "thenBranch")

_SelectionStatement_elseBranch = (Core.Name "elseBranch")

data SwitchStatement = 
  SwitchStatement {
    switchStatementValue :: Expression,
    switchStatementCases :: [CaseStatement]}
  deriving (Eq, Ord, Read, Show)

_SwitchStatement = (Core.Name "hydra.ext.cpp.syntax.SwitchStatement")

_SwitchStatement_value = (Core.Name "value")

_SwitchStatement_cases = (Core.Name "cases")

data CaseStatement = 
  CaseStatementCase CaseValue |
  CaseStatementDefault Statement
  deriving (Eq, Ord, Read, Show)

_CaseStatement = (Core.Name "hydra.ext.cpp.syntax.CaseStatement")

_CaseStatement_case = (Core.Name "case")

_CaseStatement_default = (Core.Name "default")

data CaseValue = 
  CaseValue {
    caseValueValue :: Expression,
    caseValueStatement :: Statement}
  deriving (Eq, Ord, Read, Show)

_CaseValue = (Core.Name "hydra.ext.cpp.syntax.CaseValue")

_CaseValue_value = (Core.Name "value")

_CaseValue_statement = (Core.Name "statement")

data IterationStatement = 
  IterationStatementWhile WhileStatement |
  IterationStatementDo DoStatement |
  IterationStatementFor ForStatement |
  IterationStatementRangeFor RangeForStatement
  deriving (Eq, Ord, Read, Show)

_IterationStatement = (Core.Name "hydra.ext.cpp.syntax.IterationStatement")

_IterationStatement_while = (Core.Name "while")

_IterationStatement_do = (Core.Name "do")

_IterationStatement_for = (Core.Name "for")

_IterationStatement_rangeFor = (Core.Name "rangeFor")

data WhileStatement = 
  WhileStatement {
    whileStatementCondition :: Expression,
    whileStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_WhileStatement = (Core.Name "hydra.ext.cpp.syntax.WhileStatement")

_WhileStatement_condition = (Core.Name "condition")

_WhileStatement_body = (Core.Name "body")

data DoStatement = 
  DoStatement {
    doStatementBody :: Statement,
    doStatementCondition :: Expression}
  deriving (Eq, Ord, Read, Show)

_DoStatement = (Core.Name "hydra.ext.cpp.syntax.DoStatement")

_DoStatement_body = (Core.Name "body")

_DoStatement_condition = (Core.Name "condition")

data ForStatement = 
  ForStatement {
    forStatementInit :: ForInit,
    forStatementCondition :: Expression,
    forStatementIncrement :: Expression,
    forStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_ForStatement = (Core.Name "hydra.ext.cpp.syntax.ForStatement")

_ForStatement_init = (Core.Name "init")

_ForStatement_condition = (Core.Name "condition")

_ForStatement_increment = (Core.Name "increment")

_ForStatement_body = (Core.Name "body")

data ForInit = 
  ForInitExpression Expression |
  ForInitDeclaration VariableDeclaration |
  ForInitEmpty 
  deriving (Eq, Ord, Read, Show)

_ForInit = (Core.Name "hydra.ext.cpp.syntax.ForInit")

_ForInit_expression = (Core.Name "expression")

_ForInit_declaration = (Core.Name "declaration")

_ForInit_empty = (Core.Name "empty")

data RangeForStatement = 
  RangeForStatement {
    rangeForStatementType :: TypeExpression,
    rangeForStatementVariable :: String,
    rangeForStatementRange :: Expression,
    rangeForStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_RangeForStatement = (Core.Name "hydra.ext.cpp.syntax.RangeForStatement")

_RangeForStatement_type = (Core.Name "type")

_RangeForStatement_variable = (Core.Name "variable")

_RangeForStatement_range = (Core.Name "range")

_RangeForStatement_body = (Core.Name "body")

data JumpStatement = 
  JumpStatementBreak  |
  JumpStatementContinue  |
  JumpStatementReturnValue Expression |
  JumpStatementReturnVoid  |
  JumpStatementThrow Expression
  deriving (Eq, Ord, Read, Show)

_JumpStatement = (Core.Name "hydra.ext.cpp.syntax.JumpStatement")

_JumpStatement_break = (Core.Name "break")

_JumpStatement_continue = (Core.Name "continue")

_JumpStatement_returnValue = (Core.Name "returnValue")

_JumpStatement_returnVoid = (Core.Name "returnVoid")

_JumpStatement_throw = (Core.Name "throw")

newtype ExpressionStatement = 
  ExpressionStatement {
    unExpressionStatement :: Expression}
  deriving (Eq, Ord, Read, Show)

_ExpressionStatement = (Core.Name "hydra.ext.cpp.syntax.ExpressionStatement")

data TypeExpression = 
  TypeExpressionBasic BasicType |
  TypeExpressionQualified QualifiedType |
  TypeExpressionTemplate TemplateType |
  TypeExpressionFunction FunctionType |
  TypeExpressionAuto 
  deriving (Eq, Ord, Read, Show)

_TypeExpression = (Core.Name "hydra.ext.cpp.syntax.TypeExpression")

_TypeExpression_basic = (Core.Name "basic")

_TypeExpression_qualified = (Core.Name "qualified")

_TypeExpression_template = (Core.Name "template")

_TypeExpression_function = (Core.Name "function")

_TypeExpression_auto = (Core.Name "auto")

data BasicType = 
  BasicTypeVoid  |
  BasicTypeBool  |
  BasicTypeChar  |
  BasicTypeInt  |
  BasicTypeFloat  |
  BasicTypeDouble  |
  BasicTypeString  |
  BasicTypeAuto  |
  BasicTypeNamed String
  deriving (Eq, Ord, Read, Show)

_BasicType = (Core.Name "hydra.ext.cpp.syntax.BasicType")

_BasicType_void = (Core.Name "void")

_BasicType_bool = (Core.Name "bool")

_BasicType_char = (Core.Name "char")

_BasicType_int = (Core.Name "int")

_BasicType_float = (Core.Name "float")

_BasicType_double = (Core.Name "double")

_BasicType_string = (Core.Name "string")

_BasicType_auto = (Core.Name "auto")

_BasicType_named = (Core.Name "named")

data QualifiedType = 
  QualifiedType {
    qualifiedTypeBaseType :: TypeExpression,
    qualifiedTypeQualifier :: TypeQualifier}
  deriving (Eq, Ord, Read, Show)

_QualifiedType = (Core.Name "hydra.ext.cpp.syntax.QualifiedType")

_QualifiedType_baseType = (Core.Name "baseType")

_QualifiedType_qualifier = (Core.Name "qualifier")

data TypeQualifier = 
  TypeQualifierConst  |
  TypeQualifierLvalueRef  |
  TypeQualifierRvalueRef  |
  TypeQualifierPointer 
  deriving (Eq, Ord, Read, Show)

_TypeQualifier = (Core.Name "hydra.ext.cpp.syntax.TypeQualifier")

_TypeQualifier_const = (Core.Name "const")

_TypeQualifier_lvalueRef = (Core.Name "lvalueRef")

_TypeQualifier_rvalueRef = (Core.Name "rvalueRef")

_TypeQualifier_pointer = (Core.Name "pointer")

data TemplateType = 
  TemplateType {
    templateTypeName :: String,
    templateTypeArguments :: [TemplateArgument]}
  deriving (Eq, Ord, Read, Show)

_TemplateType = (Core.Name "hydra.ext.cpp.syntax.TemplateType")

_TemplateType_name = (Core.Name "name")

_TemplateType_arguments = (Core.Name "arguments")

data TemplateArgument = 
  TemplateArgumentType TypeExpression |
  TemplateArgumentValue Expression
  deriving (Eq, Ord, Read, Show)

_TemplateArgument = (Core.Name "hydra.ext.cpp.syntax.TemplateArgument")

_TemplateArgument_type = (Core.Name "type")

_TemplateArgument_value = (Core.Name "value")

data FunctionType = 
  FunctionType {
    functionTypeReturnType :: TypeExpression,
    functionTypeParameters :: [Parameter]}
  deriving (Eq, Ord, Read, Show)

_FunctionType = (Core.Name "hydra.ext.cpp.syntax.FunctionType")

_FunctionType_returnType = (Core.Name "returnType")

_FunctionType_parameters = (Core.Name "parameters")

data Literal = 
  LiteralInteger IntegerLiteral |
  LiteralFloating FloatingLiteral |
  LiteralCharacter CharacterLiteral |
  LiteralString StringLiteral |
  LiteralBoolean BooleanLiteral |
  LiteralNull 
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.cpp.syntax.Literal")

_Literal_integer = (Core.Name "integer")

_Literal_floating = (Core.Name "floating")

_Literal_character = (Core.Name "character")

_Literal_string = (Core.Name "string")

_Literal_boolean = (Core.Name "boolean")

_Literal_null = (Core.Name "null")

data IntegerLiteral = 
  IntegerLiteralDecimal Integer |
  IntegerLiteralHexadecimal String |
  IntegerLiteralOctal String |
  IntegerLiteralBinary String
  deriving (Eq, Ord, Read, Show)

_IntegerLiteral = (Core.Name "hydra.ext.cpp.syntax.IntegerLiteral")

_IntegerLiteral_decimal = (Core.Name "decimal")

_IntegerLiteral_hexadecimal = (Core.Name "hexadecimal")

_IntegerLiteral_octal = (Core.Name "octal")

_IntegerLiteral_binary = (Core.Name "binary")

newtype FloatingLiteral = 
  FloatingLiteral {
    unFloatingLiteral :: Double}
  deriving (Eq, Ord, Read, Show)

_FloatingLiteral = (Core.Name "hydra.ext.cpp.syntax.FloatingLiteral")

newtype CharacterLiteral = 
  CharacterLiteral {
    unCharacterLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_CharacterLiteral = (Core.Name "hydra.ext.cpp.syntax.CharacterLiteral")

newtype StringLiteral = 
  StringLiteral {
    unStringLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_StringLiteral = (Core.Name "hydra.ext.cpp.syntax.StringLiteral")

newtype BooleanLiteral = 
  BooleanLiteral {
    unBooleanLiteral :: Bool}
  deriving (Eq, Ord, Read, Show)

_BooleanLiteral = (Core.Name "hydra.ext.cpp.syntax.BooleanLiteral")

data Vector = 
  Vector {
    vectorElementType :: TypeExpression,
    vectorElements :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_Vector = (Core.Name "hydra.ext.cpp.syntax.Vector")

_Vector_elementType = (Core.Name "elementType")

_Vector_elements = (Core.Name "elements")

data Map = 
  Map {
    mapKeyType :: TypeExpression,
    mapValueType :: TypeExpression,
    mapEntries :: [MapEntry]}
  deriving (Eq, Ord, Read, Show)

_Map = (Core.Name "hydra.ext.cpp.syntax.Map")

_Map_keyType = (Core.Name "keyType")

_Map_valueType = (Core.Name "valueType")

_Map_entries = (Core.Name "entries")

data MapEntry = 
  MapEntry {
    mapEntryKey :: Expression,
    mapEntryValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_MapEntry = (Core.Name "hydra.ext.cpp.syntax.MapEntry")

_MapEntry_key = (Core.Name "key")

_MapEntry_value = (Core.Name "value")

data Set = 
  Set {
    setElementType :: TypeExpression,
    setElements :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_Set = (Core.Name "hydra.ext.cpp.syntax.Set")

_Set_elementType = (Core.Name "elementType")

_Set_elements = (Core.Name "elements")

data Optional = 
  Optional {
    optionalValueType :: TypeExpression,
    optionalValue :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_Optional = (Core.Name "hydra.ext.cpp.syntax.Optional")

_Optional_valueType = (Core.Name "valueType")

_Optional_value = (Core.Name "value")

newtype Identifier = 
  Identifier {
    unIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_Identifier = (Core.Name "hydra.ext.cpp.syntax.Identifier")

data Comment = 
  Comment {
    commentText :: String,
    commentIsMultiline :: Bool}
  deriving (Eq, Ord, Read, Show)

_Comment = (Core.Name "hydra.ext.cpp.syntax.Comment")

_Comment_text = (Core.Name "text")

_Comment_isMultiline = (Core.Name "isMultiline")

data BinaryOperator = 
  BinaryOperatorPlus  |
  BinaryOperatorMinus  |
  BinaryOperatorMultiply  |
  BinaryOperatorDivide  |
  BinaryOperatorModulo  |
  BinaryOperatorBitwiseAnd  |
  BinaryOperatorBitwiseOr  |
  BinaryOperatorBitwiseXor  |
  BinaryOperatorLogicalAnd  |
  BinaryOperatorLogicalOr  |
  BinaryOperatorEqual  |
  BinaryOperatorNotEqual  |
  BinaryOperatorLess  |
  BinaryOperatorGreater  |
  BinaryOperatorLessEqual  |
  BinaryOperatorGreaterEqual  |
  BinaryOperatorLeftShift  |
  BinaryOperatorRightShift 
  deriving (Eq, Ord, Read, Show)

_BinaryOperator = (Core.Name "hydra.ext.cpp.syntax.BinaryOperator")

_BinaryOperator_plus = (Core.Name "plus")

_BinaryOperator_minus = (Core.Name "minus")

_BinaryOperator_multiply = (Core.Name "multiply")

_BinaryOperator_divide = (Core.Name "divide")

_BinaryOperator_modulo = (Core.Name "modulo")

_BinaryOperator_bitwiseAnd = (Core.Name "bitwiseAnd")

_BinaryOperator_bitwiseOr = (Core.Name "bitwiseOr")

_BinaryOperator_bitwiseXor = (Core.Name "bitwiseXor")

_BinaryOperator_logicalAnd = (Core.Name "logicalAnd")

_BinaryOperator_logicalOr = (Core.Name "logicalOr")

_BinaryOperator_equal = (Core.Name "equal")

_BinaryOperator_notEqual = (Core.Name "notEqual")

_BinaryOperator_less = (Core.Name "less")

_BinaryOperator_greater = (Core.Name "greater")

_BinaryOperator_lessEqual = (Core.Name "lessEqual")

_BinaryOperator_greaterEqual = (Core.Name "greaterEqual")

_BinaryOperator_leftShift = (Core.Name "leftShift")

_BinaryOperator_rightShift = (Core.Name "rightShift")
