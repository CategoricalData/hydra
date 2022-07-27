module Hydra.Ext.Graphql.Syntax where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

newtype Name 
  = Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra/ext/graphql/syntax.Name")

newtype IntValue 
  = IntValue {
    unIntValue :: String}
  deriving (Eq, Ord, Read, Show)

_IntValue = (Core.Name "hydra/ext/graphql/syntax.IntValue")

newtype FloatValue 
  = FloatValue {
    unFloatValue :: String}
  deriving (Eq, Ord, Read, Show)

_FloatValue = (Core.Name "hydra/ext/graphql/syntax.FloatValue")

newtype StringValue 
  = StringValue {
    unStringValue :: String}
  deriving (Eq, Ord, Read, Show)

_StringValue = (Core.Name "hydra/ext/graphql/syntax.StringValue")

newtype Document 
  = Document {
    unDocument :: [Definition]}
  deriving (Eq, Ord, Read, Show)

_Document = (Core.Name "hydra/ext/graphql/syntax.Document")

data Definition 
  = DefinitionExecutableDefinition ExecutableDefinition
  | DefinitionTypeSystemDefinitionOrExtension TypeSystemDefinitionOrExtension
  deriving (Eq, Ord, Read, Show)

_Definition = (Core.Name "hydra/ext/graphql/syntax.Definition")

_Definition_executableDefinition = (Core.FieldName "executableDefinition")

_Definition_typeSystemDefinitionOrExtension = (Core.FieldName "typeSystemDefinitionOrExtension")

newtype ExecutableDocument 
  = ExecutableDocument {
    unExecutableDocument :: [ExecutableDefinition]}
  deriving (Eq, Ord, Read, Show)

_ExecutableDocument = (Core.Name "hydra/ext/graphql/syntax.ExecutableDocument")

data ExecutableDefinition 
  = ExecutableDefinitionOperationDefinition OperationDefinition
  | ExecutableDefinitionFragmentDefinition FragmentDefinition
  deriving (Eq, Ord, Read, Show)

_ExecutableDefinition = (Core.Name "hydra/ext/graphql/syntax.ExecutableDefinition")

_ExecutableDefinition_operationDefinition = (Core.FieldName "operationDefinition")

_ExecutableDefinition_fragmentDefinition = (Core.FieldName "fragmentDefinition")

data OperationDefinition 
  = OperationDefinitionSequence OperationDefinition_Sequence
  | OperationDefinitionSelectionSet SelectionSet
  deriving (Eq, Ord, Read, Show)

_OperationDefinition = (Core.Name "hydra/ext/graphql/syntax.OperationDefinition")

_OperationDefinition_sequence = (Core.FieldName "sequence")

_OperationDefinition_selectionSet = (Core.FieldName "selectionSet")

data OperationDefinition_Sequence 
  = OperationDefinition_Sequence {
    operationDefinition_SequenceOperationType :: OperationType,
    operationDefinition_SequenceOptionName :: (Maybe Name),
    operationDefinition_SequenceOptionVariablesDefinition :: (Maybe VariablesDefinition),
    operationDefinition_SequenceOptionDirectives :: (Maybe Directives),
    operationDefinition_SequenceSelectionSet :: SelectionSet}
  deriving (Eq, Ord, Read, Show)

_OperationDefinition_Sequence = (Core.Name "hydra/ext/graphql/syntax.OperationDefinition.Sequence")

_OperationDefinition_Sequence_operationType = (Core.FieldName "operationType")

_OperationDefinition_Sequence_optionName = (Core.FieldName "optionName")

_OperationDefinition_Sequence_optionVariablesDefinition = (Core.FieldName "optionVariablesDefinition")

_OperationDefinition_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_OperationDefinition_Sequence_selectionSet = (Core.FieldName "selectionSet")

data OperationType 
  = OperationTypeQuery 
  | OperationTypeMutation 
  | OperationTypeSubscription 
  deriving (Eq, Ord, Read, Show)

_OperationType = (Core.Name "hydra/ext/graphql/syntax.OperationType")

_OperationType_query = (Core.FieldName "query")

_OperationType_mutation = (Core.FieldName "mutation")

_OperationType_subscription = (Core.FieldName "subscription")

data SelectionSet 
  = SelectionSet {
    selectionSetStarSelection :: [Selection]}
  deriving (Eq, Ord, Read, Show)

_SelectionSet = (Core.Name "hydra/ext/graphql/syntax.SelectionSet")

_SelectionSet_starSelection = (Core.FieldName "starSelection")

data Selection 
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment
  deriving (Eq, Ord, Read, Show)

_Selection = (Core.Name "hydra/ext/graphql/syntax.Selection")

_Selection_field = (Core.FieldName "field")

_Selection_fragmentSpread = (Core.FieldName "fragmentSpread")

_Selection_inlineFragment = (Core.FieldName "inlineFragment")

data Field 
  = Field {
    fieldOptionAlias :: (Maybe Alias),
    fieldName :: Name,
    fieldOptionArguments :: (Maybe Arguments),
    fieldOptionDirectives :: (Maybe Directives),
    fieldOptionSelectionSet :: (Maybe SelectionSet)}
  deriving (Eq, Ord, Read, Show)

_Field = (Core.Name "hydra/ext/graphql/syntax.Field")

_Field_optionAlias = (Core.FieldName "optionAlias")

_Field_name = (Core.FieldName "name")

_Field_optionArguments = (Core.FieldName "optionArguments")

_Field_optionDirectives = (Core.FieldName "optionDirectives")

_Field_optionSelectionSet = (Core.FieldName "optionSelectionSet")

data Alias 
  = AliasName Name
  | AliasColon 
  deriving (Eq, Ord, Read, Show)

_Alias = (Core.Name "hydra/ext/graphql/syntax.Alias")

_Alias_name = (Core.FieldName "name")

_Alias_colon = (Core.FieldName "colon")

data Arguments 
  = Arguments {
    argumentsStarArgument :: [Argument]}
  deriving (Eq, Ord, Read, Show)

_Arguments = (Core.Name "hydra/ext/graphql/syntax.Arguments")

_Arguments_starArgument = (Core.FieldName "starArgument")

data Argument 
  = Argument {
    argumentName :: Name,
    argumentValue :: Value}
  deriving (Eq, Ord, Read, Show)

_Argument = (Core.Name "hydra/ext/graphql/syntax.Argument")

_Argument_name = (Core.FieldName "name")

_Argument_value = (Core.FieldName "value")

data FragmentSpread 
  = FragmentSpread {
    fragmentSpreadFragmentName :: FragmentName,
    fragmentSpreadOptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_FragmentSpread = (Core.Name "hydra/ext/graphql/syntax.FragmentSpread")

_FragmentSpread_fragmentName = (Core.FieldName "fragmentName")

_FragmentSpread_optionDirectives = (Core.FieldName "optionDirectives")

data InlineFragment 
  = InlineFragment {
    inlineFragmentOptionTypeCondition :: (Maybe TypeCondition),
    inlineFragmentOptionDirectives :: (Maybe Directives),
    inlineFragmentSelectionSet :: SelectionSet}
  deriving (Eq, Ord, Read, Show)

_InlineFragment = (Core.Name "hydra/ext/graphql/syntax.InlineFragment")

_InlineFragment_optionTypeCondition = (Core.FieldName "optionTypeCondition")

_InlineFragment_optionDirectives = (Core.FieldName "optionDirectives")

_InlineFragment_selectionSet = (Core.FieldName "selectionSet")

data FragmentDefinition 
  = FragmentDefinition {
    fragmentDefinitionFragmentName :: FragmentName,
    fragmentDefinitionTypeCondition :: TypeCondition,
    fragmentDefinitionOptionDirectives :: (Maybe Directives),
    fragmentDefinitionSelectionSet :: SelectionSet}
  deriving (Eq, Ord, Read, Show)

_FragmentDefinition = (Core.Name "hydra/ext/graphql/syntax.FragmentDefinition")

_FragmentDefinition_fragmentName = (Core.FieldName "fragmentName")

_FragmentDefinition_typeCondition = (Core.FieldName "typeCondition")

_FragmentDefinition_optionDirectives = (Core.FieldName "optionDirectives")

_FragmentDefinition_selectionSet = (Core.FieldName "selectionSet")

newtype FragmentName 
  = FragmentName {
    unFragmentName :: Name}
  deriving (Eq, Ord, Read, Show)

_FragmentName = (Core.Name "hydra/ext/graphql/syntax.FragmentName")

data TypeCondition 
  = TypeConditionOn 
  | TypeConditionNamedType NamedType
  deriving (Eq, Ord, Read, Show)

_TypeCondition = (Core.Name "hydra/ext/graphql/syntax.TypeCondition")

_TypeCondition_on = (Core.FieldName "on")

_TypeCondition_namedType = (Core.FieldName "namedType")

data Value 
  = ValueVariable Variable
  | ValueIntValue IntValue
  | ValueFloatValue FloatValue
  | ValueStringValue StringValue
  | ValueBooleanValue BooleanValue
  | ValueNullValue NullValue
  | ValueEnumValue EnumValue
  | ValueListValue ListValue
  | ValueObjectValue ObjectValue
  deriving (Eq, Ord, Read, Show)

_Value = (Core.Name "hydra/ext/graphql/syntax.Value")

_Value_variable = (Core.FieldName "variable")

_Value_intValue = (Core.FieldName "intValue")

_Value_floatValue = (Core.FieldName "floatValue")

_Value_stringValue = (Core.FieldName "stringValue")

_Value_booleanValue = (Core.FieldName "booleanValue")

_Value_nullValue = (Core.FieldName "nullValue")

_Value_enumValue = (Core.FieldName "enumValue")

_Value_listValue = (Core.FieldName "listValue")

_Value_objectValue = (Core.FieldName "objectValue")

data BooleanValue 
  = BooleanValueTrue 
  | BooleanValueFalse 
  deriving (Eq, Ord, Read, Show)

_BooleanValue = (Core.Name "hydra/ext/graphql/syntax.BooleanValue")

_BooleanValue_true = (Core.FieldName "true")

_BooleanValue_false = (Core.FieldName "false")

data NullValue 
  = NullValue {}
  deriving (Eq, Ord, Read, Show)

_NullValue = (Core.Name "hydra/ext/graphql/syntax.NullValue")

data EnumValue 
  = EnumValue {
    enumValueName :: Name}
  deriving (Eq, Ord, Read, Show)

_EnumValue = (Core.Name "hydra/ext/graphql/syntax.EnumValue")

_EnumValue_name = (Core.FieldName "name")

data ListValue 
  = ListValueSequence ListValue_Sequence
  | ListValueSequence2 ListValue_Sequence2
  deriving (Eq, Ord, Read, Show)

_ListValue = (Core.Name "hydra/ext/graphql/syntax.ListValue")

_ListValue_sequence = (Core.FieldName "sequence")

_ListValue_sequence2 = (Core.FieldName "sequence2")

data ListValue_Sequence 
  = ListValue_Sequence {}
  deriving (Eq, Ord, Read, Show)

_ListValue_Sequence = (Core.Name "hydra/ext/graphql/syntax.ListValue.Sequence")

data ListValue_Sequence2 
  = ListValue_Sequence2 {
    listValue_Sequence2StarValue :: [Value]}
  deriving (Eq, Ord, Read, Show)

_ListValue_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.ListValue.Sequence2")

_ListValue_Sequence2_starValue = (Core.FieldName "starValue")

data ObjectValue 
  = ObjectValueSequence ObjectValue_Sequence
  | ObjectValueSequence2 ObjectValue_Sequence2
  deriving (Eq, Ord, Read, Show)

_ObjectValue = (Core.Name "hydra/ext/graphql/syntax.ObjectValue")

_ObjectValue_sequence = (Core.FieldName "sequence")

_ObjectValue_sequence2 = (Core.FieldName "sequence2")

data ObjectValue_Sequence 
  = ObjectValue_Sequence {}
  deriving (Eq, Ord, Read, Show)

_ObjectValue_Sequence = (Core.Name "hydra/ext/graphql/syntax.ObjectValue.Sequence")

data ObjectValue_Sequence2 
  = ObjectValue_Sequence2 {
    objectValue_Sequence2StarObjectField :: [ObjectField]}
  deriving (Eq, Ord, Read, Show)

_ObjectValue_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.ObjectValue.Sequence2")

_ObjectValue_Sequence2_starObjectField = (Core.FieldName "starObjectField")

data ObjectField 
  = ObjectField {
    objectFieldName :: Name,
    objectFieldValue :: Value}
  deriving (Eq, Ord, Read, Show)

_ObjectField = (Core.Name "hydra/ext/graphql/syntax.ObjectField")

_ObjectField_name = (Core.FieldName "name")

_ObjectField_value = (Core.FieldName "value")

data VariablesDefinition 
  = VariablesDefinition {
    variablesDefinitionVariable :: Variable,
    variablesDefinitionType :: Type,
    variablesDefinitionOptionDefaultValue :: (Maybe DefaultValue),
    variablesDefinitionOptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_VariablesDefinition = (Core.Name "hydra/ext/graphql/syntax.VariablesDefinition")

_VariablesDefinition_variable = (Core.FieldName "variable")

_VariablesDefinition_type = (Core.FieldName "type")

_VariablesDefinition_optionDefaultValue = (Core.FieldName "optionDefaultValue")

_VariablesDefinition_optionDirectives = (Core.FieldName "optionDirectives")

newtype Variable 
  = Variable {
    unVariable :: Name}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra/ext/graphql/syntax.Variable")

data DefaultValue 
  = DefaultValue {
    defaultValueValue :: Value}
  deriving (Eq, Ord, Read, Show)

_DefaultValue = (Core.Name "hydra/ext/graphql/syntax.DefaultValue")

_DefaultValue_value = (Core.FieldName "value")

data Type 
  = TypeNamedType NamedType
  | TypeListType ListType
  | TypeNonNullType NonNullType
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra/ext/graphql/syntax.Type")

_Type_namedType = (Core.FieldName "namedType")

_Type_listType = (Core.FieldName "listType")

_Type_nonNullType = (Core.FieldName "nonNullType")

newtype NamedType 
  = NamedType {
    unNamedType :: Name}
  deriving (Eq, Ord, Read, Show)

_NamedType = (Core.Name "hydra/ext/graphql/syntax.NamedType")

data ListType 
  = ListType {
    listTypeType :: Type}
  deriving (Eq, Ord, Read, Show)

_ListType = (Core.Name "hydra/ext/graphql/syntax.ListType")

_ListType_type = (Core.FieldName "type")

data NonNullType 
  = NonNullTypeSequence NonNullType_Sequence
  | NonNullTypeSequence2 NonNullType_Sequence2
  deriving (Eq, Ord, Read, Show)

_NonNullType = (Core.Name "hydra/ext/graphql/syntax.NonNullType")

_NonNullType_sequence = (Core.FieldName "sequence")

_NonNullType_sequence2 = (Core.FieldName "sequence2")

data NonNullType_Sequence 
  = NonNullType_Sequence {
    nonNullType_SequenceNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_NonNullType_Sequence = (Core.Name "hydra/ext/graphql/syntax.NonNullType.Sequence")

_NonNullType_Sequence_namedType = (Core.FieldName "namedType")

data NonNullType_Sequence2 
  = NonNullType_Sequence2 {
    nonNullType_Sequence2ListType :: ListType}
  deriving (Eq, Ord, Read, Show)

_NonNullType_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.NonNullType.Sequence2")

_NonNullType_Sequence2_listType = (Core.FieldName "listType")

newtype Directives 
  = Directives {
    unDirectives :: [Directive]}
  deriving (Eq, Ord, Read, Show)

_Directives = (Core.Name "hydra/ext/graphql/syntax.Directives")

data Directive 
  = Directive {
    directiveName :: Name,
    directiveOptionArguments :: (Maybe Arguments)}
  deriving (Eq, Ord, Read, Show)

_Directive = (Core.Name "hydra/ext/graphql/syntax.Directive")

_Directive_name = (Core.FieldName "name")

_Directive_optionArguments = (Core.FieldName "optionArguments")

newtype TypeSystemDocment 
  = TypeSystemDocment {
    unTypeSystemDocment :: [TypeSystemDefinition]}
  deriving (Eq, Ord, Read, Show)

_TypeSystemDocment = (Core.Name "hydra/ext/graphql/syntax.TypeSystemDocment")

data TypeSystemDefinition 
  = TypeSystemDefinitionSchemaDefinition SchemaDefinition
  | TypeSystemDefinitionTypeDefinition TypeDefinition
  | TypeSystemDefinitionDirectiveDefinition DirectiveDefinition
  deriving (Eq, Ord, Read, Show)

_TypeSystemDefinition = (Core.Name "hydra/ext/graphql/syntax.TypeSystemDefinition")

_TypeSystemDefinition_schemaDefinition = (Core.FieldName "schemaDefinition")

_TypeSystemDefinition_typeDefinition = (Core.FieldName "typeDefinition")

_TypeSystemDefinition_directiveDefinition = (Core.FieldName "directiveDefinition")

newtype TypeSystemExtensionDocument 
  = TypeSystemExtensionDocument {
    unTypeSystemExtensionDocument :: [TypeSystemDefinitionOrExtension]}
  deriving (Eq, Ord, Read, Show)

_TypeSystemExtensionDocument = (Core.Name "hydra/ext/graphql/syntax.TypeSystemExtensionDocument")

data TypeSystemDefinitionOrExtension 
  = TypeSystemDefinitionOrExtensionTypeSystemDefinition TypeSystemDefinition
  | TypeSystemDefinitionOrExtensionTypeSystemExtension TypeSystemExtension
  deriving (Eq, Ord, Read, Show)

_TypeSystemDefinitionOrExtension = (Core.Name "hydra/ext/graphql/syntax.TypeSystemDefinitionOrExtension")

_TypeSystemDefinitionOrExtension_typeSystemDefinition = (Core.FieldName "typeSystemDefinition")

_TypeSystemDefinitionOrExtension_typeSystemExtension = (Core.FieldName "typeSystemExtension")

data TypeSystemExtension 
  = TypeSystemExtensionSchemaExtension SchemaExtension
  | TypeSystemExtensionTypeExtension TypeExtension
  deriving (Eq, Ord, Read, Show)

_TypeSystemExtension = (Core.Name "hydra/ext/graphql/syntax.TypeSystemExtension")

_TypeSystemExtension_schemaExtension = (Core.FieldName "schemaExtension")

_TypeSystemExtension_typeExtension = (Core.FieldName "typeExtension")

data SchemaDefinition 
  = SchemaDefinition {
    schemaDefinitionOptionDescription :: (Maybe Description),
    schemaDefinitionOptionDirectives :: (Maybe Directives),
    schemaDefinitionRootOperationTypeDefinition :: RootOperationTypeDefinition}
  deriving (Eq, Ord, Read, Show)

_SchemaDefinition = (Core.Name "hydra/ext/graphql/syntax.SchemaDefinition")

_SchemaDefinition_optionDescription = (Core.FieldName "optionDescription")

_SchemaDefinition_optionDirectives = (Core.FieldName "optionDirectives")

_SchemaDefinition_rootOperationTypeDefinition = (Core.FieldName "rootOperationTypeDefinition")

data SchemaExtension 
  = SchemaExtensionSequence SchemaExtension_Sequence
  | SchemaExtensionSequence2 SchemaExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_SchemaExtension = (Core.Name "hydra/ext/graphql/syntax.SchemaExtension")

_SchemaExtension_sequence = (Core.FieldName "sequence")

_SchemaExtension_sequence2 = (Core.FieldName "sequence2")

data SchemaExtension_Sequence 
  = SchemaExtension_Sequence {
    schemaExtension_SequenceOptionDirectives :: (Maybe Directives),
    schemaExtension_SequenceRootOperationTypeDefinition :: RootOperationTypeDefinition}
  deriving (Eq, Ord, Read, Show)

_SchemaExtension_Sequence = (Core.Name "hydra/ext/graphql/syntax.SchemaExtension.Sequence")

_SchemaExtension_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_SchemaExtension_Sequence_rootOperationTypeDefinition = (Core.FieldName "rootOperationTypeDefinition")

data SchemaExtension_Sequence2 
  = SchemaExtension_Sequence2 {
    schemaExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_SchemaExtension_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.SchemaExtension.Sequence2")

_SchemaExtension_Sequence2_directives = (Core.FieldName "directives")

data RootOperationTypeDefinition 
  = RootOperationTypeDefinition {
    rootOperationTypeDefinitionOperationType :: OperationType,
    rootOperationTypeDefinitionNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_RootOperationTypeDefinition = (Core.Name "hydra/ext/graphql/syntax.RootOperationTypeDefinition")

_RootOperationTypeDefinition_operationType = (Core.FieldName "operationType")

_RootOperationTypeDefinition_namedType = (Core.FieldName "namedType")

newtype Description 
  = Description {
    unDescription :: StringValue}
  deriving (Eq, Ord, Read, Show)

_Description = (Core.Name "hydra/ext/graphql/syntax.Description")

data TypeDefinition 
  = TypeDefinitionScalarTypeDefinition ScalarTypeDefinition
  | TypeDefinitionObjectTypeDefinition ObjectTypeDefinition
  | TypeDefinitionInterfaceTypeDefinition InterfaceTypeDefinition
  | TypeDefinitionUnionTypeDefinition UnionTypeDefinition
  | TypeDefinitionEnumTypeDefinition EnumTypeDefinition
  | TypeDefinitionInputObjectTypeDefinition InputObjectTypeDefinition
  deriving (Eq, Ord, Read, Show)

_TypeDefinition = (Core.Name "hydra/ext/graphql/syntax.TypeDefinition")

_TypeDefinition_scalarTypeDefinition = (Core.FieldName "scalarTypeDefinition")

_TypeDefinition_objectTypeDefinition = (Core.FieldName "objectTypeDefinition")

_TypeDefinition_interfaceTypeDefinition = (Core.FieldName "interfaceTypeDefinition")

_TypeDefinition_unionTypeDefinition = (Core.FieldName "unionTypeDefinition")

_TypeDefinition_enumTypeDefinition = (Core.FieldName "enumTypeDefinition")

_TypeDefinition_inputObjectTypeDefinition = (Core.FieldName "inputObjectTypeDefinition")

data TypeExtension 
  = TypeExtensionScalarTypeExtension ScalarTypeExtension
  | TypeExtensionObjectTypeExtension ObjectTypeExtension
  | TypeExtensionInterfaceTypeExtension InterfaceTypeExtension
  | TypeExtensionUnionTypeExtension UnionTypeExtension
  | TypeExtensionEnumTypeExtension EnumTypeExtension
  | TypeExtensionInputObjectTypeExtension InputObjectTypeExtension
  deriving (Eq, Ord, Read, Show)

_TypeExtension = (Core.Name "hydra/ext/graphql/syntax.TypeExtension")

_TypeExtension_scalarTypeExtension = (Core.FieldName "scalarTypeExtension")

_TypeExtension_objectTypeExtension = (Core.FieldName "objectTypeExtension")

_TypeExtension_interfaceTypeExtension = (Core.FieldName "interfaceTypeExtension")

_TypeExtension_unionTypeExtension = (Core.FieldName "unionTypeExtension")

_TypeExtension_enumTypeExtension = (Core.FieldName "enumTypeExtension")

_TypeExtension_inputObjectTypeExtension = (Core.FieldName "inputObjectTypeExtension")

data ScalarTypeDefinition 
  = ScalarTypeDefinition {
    scalarTypeDefinitionOptionDescription :: (Maybe Description),
    scalarTypeDefinitionName :: Name,
    scalarTypeDefinitionOptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_ScalarTypeDefinition = (Core.Name "hydra/ext/graphql/syntax.ScalarTypeDefinition")

_ScalarTypeDefinition_optionDescription = (Core.FieldName "optionDescription")

_ScalarTypeDefinition_name = (Core.FieldName "name")

_ScalarTypeDefinition_optionDirectives = (Core.FieldName "optionDirectives")

data ScalarTypeExtension 
  = ScalarTypeExtension {
    scalarTypeExtensionName :: Name,
    scalarTypeExtensionDirectives :: Directives}
  deriving (Eq, Ord, Read, Show)

_ScalarTypeExtension = (Core.Name "hydra/ext/graphql/syntax.ScalarTypeExtension")

_ScalarTypeExtension_name = (Core.FieldName "name")

_ScalarTypeExtension_directives = (Core.FieldName "directives")

data ObjectTypeDefinition 
  = ObjectTypeDefinitionSequence ObjectTypeDefinition_Sequence
  | ObjectTypeDefinitionSequence2 ObjectTypeDefinition_Sequence2
  deriving (Eq, Ord, Read, Show)

_ObjectTypeDefinition = (Core.Name "hydra/ext/graphql/syntax.ObjectTypeDefinition")

_ObjectTypeDefinition_sequence = (Core.FieldName "sequence")

_ObjectTypeDefinition_sequence2 = (Core.FieldName "sequence2")

data ObjectTypeDefinition_Sequence 
  = ObjectTypeDefinition_Sequence {
    objectTypeDefinition_SequenceOptionDescription :: (Maybe Description),
    objectTypeDefinition_SequenceName :: Name,
    objectTypeDefinition_SequenceOptionImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeDefinition_SequenceOptionDirectives :: (Maybe Directives),
    objectTypeDefinition_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeDefinition_Sequence = (Core.Name "hydra/ext/graphql/syntax.ObjectTypeDefinition.Sequence")

_ObjectTypeDefinition_Sequence_optionDescription = (Core.FieldName "optionDescription")

_ObjectTypeDefinition_Sequence_name = (Core.FieldName "name")

_ObjectTypeDefinition_Sequence_optionImplementsInterfaces = (Core.FieldName "optionImplementsInterfaces")

_ObjectTypeDefinition_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_ObjectTypeDefinition_Sequence_fieldsDefinition = (Core.FieldName "fieldsDefinition")

data ObjectTypeDefinition_Sequence2 
  = ObjectTypeDefinition_Sequence2 {
    objectTypeDefinition_Sequence2OptionDescription :: (Maybe Description),
    objectTypeDefinition_Sequence2Name :: Name,
    objectTypeDefinition_Sequence2OptionImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeDefinition_Sequence2OptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeDefinition_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.ObjectTypeDefinition.Sequence2")

_ObjectTypeDefinition_Sequence2_optionDescription = (Core.FieldName "optionDescription")

_ObjectTypeDefinition_Sequence2_name = (Core.FieldName "name")

_ObjectTypeDefinition_Sequence2_optionImplementsInterfaces = (Core.FieldName "optionImplementsInterfaces")

_ObjectTypeDefinition_Sequence2_optionDirectives = (Core.FieldName "optionDirectives")

data ObjectTypeExtension 
  = ObjectTypeExtensionSequence ObjectTypeExtension_Sequence
  | ObjectTypeExtensionSequence2 ObjectTypeExtension_Sequence2
  | ObjectTypeExtensionSequence3 ObjectTypeExtension_Sequence3
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension = (Core.Name "hydra/ext/graphql/syntax.ObjectTypeExtension")

_ObjectTypeExtension_sequence = (Core.FieldName "sequence")

_ObjectTypeExtension_sequence2 = (Core.FieldName "sequence2")

_ObjectTypeExtension_sequence3 = (Core.FieldName "sequence3")

data ObjectTypeExtension_Sequence 
  = ObjectTypeExtension_Sequence {
    objectTypeExtension_SequenceName :: Name,
    objectTypeExtension_SequenceOptionImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeExtension_SequenceOptionDirectives :: (Maybe Directives),
    objectTypeExtension_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension_Sequence = (Core.Name "hydra/ext/graphql/syntax.ObjectTypeExtension.Sequence")

_ObjectTypeExtension_Sequence_name = (Core.FieldName "name")

_ObjectTypeExtension_Sequence_optionImplementsInterfaces = (Core.FieldName "optionImplementsInterfaces")

_ObjectTypeExtension_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_ObjectTypeExtension_Sequence_fieldsDefinition = (Core.FieldName "fieldsDefinition")

data ObjectTypeExtension_Sequence2 
  = ObjectTypeExtension_Sequence2 {
    objectTypeExtension_Sequence2Name :: Name,
    objectTypeExtension_Sequence2OptionImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeExtension_Sequence2OptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.ObjectTypeExtension.Sequence2")

_ObjectTypeExtension_Sequence2_name = (Core.FieldName "name")

_ObjectTypeExtension_Sequence2_optionImplementsInterfaces = (Core.FieldName "optionImplementsInterfaces")

_ObjectTypeExtension_Sequence2_optionDirectives = (Core.FieldName "optionDirectives")

data ObjectTypeExtension_Sequence3 
  = ObjectTypeExtension_Sequence3 {
    objectTypeExtension_Sequence3Name :: Name,
    objectTypeExtension_Sequence3ImplementsInterfaces :: ImplementsInterfaces}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension_Sequence3 = (Core.Name "hydra/ext/graphql/syntax.ObjectTypeExtension.Sequence3")

_ObjectTypeExtension_Sequence3_name = (Core.FieldName "name")

_ObjectTypeExtension_Sequence3_implementsInterfaces = (Core.FieldName "implementsInterfaces")

data ImplementsInterfaces 
  = ImplementsInterfacesSequence ImplementsInterfaces_Sequence
  | ImplementsInterfacesSequence2 ImplementsInterfaces_Sequence2
  deriving (Eq, Ord, Read, Show)

_ImplementsInterfaces = (Core.Name "hydra/ext/graphql/syntax.ImplementsInterfaces")

_ImplementsInterfaces_sequence = (Core.FieldName "sequence")

_ImplementsInterfaces_sequence2 = (Core.FieldName "sequence2")

data ImplementsInterfaces_Sequence 
  = ImplementsInterfaces_Sequence {
    implementsInterfaces_SequenceImplementsInterfaces :: ImplementsInterfaces,
    implementsInterfaces_SequenceNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_ImplementsInterfaces_Sequence = (Core.Name "hydra/ext/graphql/syntax.ImplementsInterfaces.Sequence")

_ImplementsInterfaces_Sequence_implementsInterfaces = (Core.FieldName "implementsInterfaces")

_ImplementsInterfaces_Sequence_namedType = (Core.FieldName "namedType")

data ImplementsInterfaces_Sequence2 
  = ImplementsInterfaces_Sequence2 {
    implementsInterfaces_Sequence2OptionAmp :: (Maybe ()),
    implementsInterfaces_Sequence2NamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_ImplementsInterfaces_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.ImplementsInterfaces.Sequence2")

_ImplementsInterfaces_Sequence2_optionAmp = (Core.FieldName "optionAmp")

_ImplementsInterfaces_Sequence2_namedType = (Core.FieldName "namedType")

data FieldsDefinition 
  = FieldsDefinition {
    fieldsDefinitionStarFieldDefinition :: [FieldDefinition]}
  deriving (Eq, Ord, Read, Show)

_FieldsDefinition = (Core.Name "hydra/ext/graphql/syntax.FieldsDefinition")

_FieldsDefinition_starFieldDefinition = (Core.FieldName "starFieldDefinition")

data FieldDefinition 
  = FieldDefinition {
    fieldDefinitionOptionDescription :: (Maybe Description),
    fieldDefinitionName :: Name,
    fieldDefinitionOptionArgumentsDefinition :: (Maybe ArgumentsDefinition),
    fieldDefinitionType :: Type,
    fieldDefinitionOptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_FieldDefinition = (Core.Name "hydra/ext/graphql/syntax.FieldDefinition")

_FieldDefinition_optionDescription = (Core.FieldName "optionDescription")

_FieldDefinition_name = (Core.FieldName "name")

_FieldDefinition_optionArgumentsDefinition = (Core.FieldName "optionArgumentsDefinition")

_FieldDefinition_type = (Core.FieldName "type")

_FieldDefinition_optionDirectives = (Core.FieldName "optionDirectives")

data ArgumentsDefinition 
  = ArgumentsDefinition {
    argumentsDefinitionStarInputValueDefinition :: [InputValueDefinition]}
  deriving (Eq, Ord, Read, Show)

_ArgumentsDefinition = (Core.Name "hydra/ext/graphql/syntax.ArgumentsDefinition")

_ArgumentsDefinition_starInputValueDefinition = (Core.FieldName "starInputValueDefinition")

data InputValueDefinition 
  = InputValueDefinition {
    inputValueDefinitionOptionDescription :: (Maybe Description),
    inputValueDefinitionName :: Name,
    inputValueDefinitionType :: Type,
    inputValueDefinitionOptionDefaultValue :: (Maybe DefaultValue),
    inputValueDefinitionOptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_InputValueDefinition = (Core.Name "hydra/ext/graphql/syntax.InputValueDefinition")

_InputValueDefinition_optionDescription = (Core.FieldName "optionDescription")

_InputValueDefinition_name = (Core.FieldName "name")

_InputValueDefinition_type = (Core.FieldName "type")

_InputValueDefinition_optionDefaultValue = (Core.FieldName "optionDefaultValue")

_InputValueDefinition_optionDirectives = (Core.FieldName "optionDirectives")

data InterfaceTypeDefinition 
  = InterfaceTypeDefinitionSequence InterfaceTypeDefinition_Sequence
  | InterfaceTypeDefinitionSequence2 InterfaceTypeDefinition_Sequence2
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeDefinition = (Core.Name "hydra/ext/graphql/syntax.InterfaceTypeDefinition")

_InterfaceTypeDefinition_sequence = (Core.FieldName "sequence")

_InterfaceTypeDefinition_sequence2 = (Core.FieldName "sequence2")

data InterfaceTypeDefinition_Sequence 
  = InterfaceTypeDefinition_Sequence {
    interfaceTypeDefinition_SequenceOptionDescription :: (Maybe Description),
    interfaceTypeDefinition_SequenceName :: Name,
    interfaceTypeDefinition_SequenceOptionImplementsInterfaces :: (Maybe ImplementsInterfaces),
    interfaceTypeDefinition_SequenceOptionDirectives :: (Maybe Directives),
    interfaceTypeDefinition_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeDefinition_Sequence = (Core.Name "hydra/ext/graphql/syntax.InterfaceTypeDefinition.Sequence")

_InterfaceTypeDefinition_Sequence_optionDescription = (Core.FieldName "optionDescription")

_InterfaceTypeDefinition_Sequence_name = (Core.FieldName "name")

_InterfaceTypeDefinition_Sequence_optionImplementsInterfaces = (Core.FieldName "optionImplementsInterfaces")

_InterfaceTypeDefinition_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_InterfaceTypeDefinition_Sequence_fieldsDefinition = (Core.FieldName "fieldsDefinition")

data InterfaceTypeDefinition_Sequence2 
  = InterfaceTypeDefinition_Sequence2 {
    interfaceTypeDefinition_Sequence2OptionDescription :: (Maybe Description),
    interfaceTypeDefinition_Sequence2Name :: Name,
    interfaceTypeDefinition_Sequence2ImplementsInterfaces :: ImplementsInterfaces,
    interfaceTypeDefinition_Sequence2OptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeDefinition_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.InterfaceTypeDefinition.Sequence2")

_InterfaceTypeDefinition_Sequence2_optionDescription = (Core.FieldName "optionDescription")

_InterfaceTypeDefinition_Sequence2_name = (Core.FieldName "name")

_InterfaceTypeDefinition_Sequence2_implementsInterfaces = (Core.FieldName "implementsInterfaces")

_InterfaceTypeDefinition_Sequence2_optionDirectives = (Core.FieldName "optionDirectives")

data InterfaceTypeExtension 
  = InterfaceTypeExtensionSequence InterfaceTypeExtension_Sequence
  | InterfaceTypeExtensionSequence2 InterfaceTypeExtension_Sequence2
  | InterfaceTypeExtensionSequence3 InterfaceTypeExtension_Sequence3
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension = (Core.Name "hydra/ext/graphql/syntax.InterfaceTypeExtension")

_InterfaceTypeExtension_sequence = (Core.FieldName "sequence")

_InterfaceTypeExtension_sequence2 = (Core.FieldName "sequence2")

_InterfaceTypeExtension_sequence3 = (Core.FieldName "sequence3")

data InterfaceTypeExtension_Sequence 
  = InterfaceTypeExtension_Sequence {
    interfaceTypeExtension_SequenceName :: Name,
    interfaceTypeExtension_SequenceOptionImplementsInterfaces :: (Maybe ImplementsInterfaces),
    interfaceTypeExtension_SequenceOptionDirectives :: (Maybe Directives),
    interfaceTypeExtension_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension_Sequence = (Core.Name "hydra/ext/graphql/syntax.InterfaceTypeExtension.Sequence")

_InterfaceTypeExtension_Sequence_name = (Core.FieldName "name")

_InterfaceTypeExtension_Sequence_optionImplementsInterfaces = (Core.FieldName "optionImplementsInterfaces")

_InterfaceTypeExtension_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_InterfaceTypeExtension_Sequence_fieldsDefinition = (Core.FieldName "fieldsDefinition")

data InterfaceTypeExtension_Sequence2 
  = InterfaceTypeExtension_Sequence2 {
    interfaceTypeExtension_Sequence2Name :: Name,
    interfaceTypeExtension_Sequence2OptionImplementsInterfaces :: (Maybe ImplementsInterfaces),
    interfaceTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.InterfaceTypeExtension.Sequence2")

_InterfaceTypeExtension_Sequence2_name = (Core.FieldName "name")

_InterfaceTypeExtension_Sequence2_optionImplementsInterfaces = (Core.FieldName "optionImplementsInterfaces")

_InterfaceTypeExtension_Sequence2_directives = (Core.FieldName "directives")

data InterfaceTypeExtension_Sequence3 
  = InterfaceTypeExtension_Sequence3 {
    interfaceTypeExtension_Sequence3Name :: Name,
    interfaceTypeExtension_Sequence3ImplementsInterfaces :: ImplementsInterfaces}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension_Sequence3 = (Core.Name "hydra/ext/graphql/syntax.InterfaceTypeExtension.Sequence3")

_InterfaceTypeExtension_Sequence3_name = (Core.FieldName "name")

_InterfaceTypeExtension_Sequence3_implementsInterfaces = (Core.FieldName "implementsInterfaces")

data UnionTypeDefinition 
  = UnionTypeDefinition {
    unionTypeDefinitionOptionDescription :: (Maybe Description),
    unionTypeDefinitionName :: Name,
    unionTypeDefinitionOptionDirectives :: (Maybe Directives),
    unionTypeDefinitionOptionUnionMemberTypes :: (Maybe UnionMemberTypes)}
  deriving (Eq, Ord, Read, Show)

_UnionTypeDefinition = (Core.Name "hydra/ext/graphql/syntax.UnionTypeDefinition")

_UnionTypeDefinition_optionDescription = (Core.FieldName "optionDescription")

_UnionTypeDefinition_name = (Core.FieldName "name")

_UnionTypeDefinition_optionDirectives = (Core.FieldName "optionDirectives")

_UnionTypeDefinition_optionUnionMemberTypes = (Core.FieldName "optionUnionMemberTypes")

data UnionMemberTypes 
  = UnionMemberTypesSequence UnionMemberTypes_Sequence
  | UnionMemberTypesSequence2 UnionMemberTypes_Sequence2
  deriving (Eq, Ord, Read, Show)

_UnionMemberTypes = (Core.Name "hydra/ext/graphql/syntax.UnionMemberTypes")

_UnionMemberTypes_sequence = (Core.FieldName "sequence")

_UnionMemberTypes_sequence2 = (Core.FieldName "sequence2")

data UnionMemberTypes_Sequence 
  = UnionMemberTypes_Sequence {
    unionMemberTypes_SequenceUnionMemberTypes :: UnionMemberTypes,
    unionMemberTypes_SequenceNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_UnionMemberTypes_Sequence = (Core.Name "hydra/ext/graphql/syntax.UnionMemberTypes.Sequence")

_UnionMemberTypes_Sequence_unionMemberTypes = (Core.FieldName "unionMemberTypes")

_UnionMemberTypes_Sequence_namedType = (Core.FieldName "namedType")

data UnionMemberTypes_Sequence2 
  = UnionMemberTypes_Sequence2 {
    unionMemberTypes_Sequence2OptionOr :: (Maybe ()),
    unionMemberTypes_Sequence2NamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_UnionMemberTypes_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.UnionMemberTypes.Sequence2")

_UnionMemberTypes_Sequence2_optionOr = (Core.FieldName "optionOr")

_UnionMemberTypes_Sequence2_namedType = (Core.FieldName "namedType")

data UnionTypeExtension 
  = UnionTypeExtensionSequence UnionTypeExtension_Sequence
  | UnionTypeExtensionSequence2 UnionTypeExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_UnionTypeExtension = (Core.Name "hydra/ext/graphql/syntax.UnionTypeExtension")

_UnionTypeExtension_sequence = (Core.FieldName "sequence")

_UnionTypeExtension_sequence2 = (Core.FieldName "sequence2")

data UnionTypeExtension_Sequence 
  = UnionTypeExtension_Sequence {
    unionTypeExtension_SequenceName :: Name,
    unionTypeExtension_SequenceOptionDirectives :: (Maybe Directives),
    unionTypeExtension_SequenceUnionMemberTypes :: UnionMemberTypes}
  deriving (Eq, Ord, Read, Show)

_UnionTypeExtension_Sequence = (Core.Name "hydra/ext/graphql/syntax.UnionTypeExtension.Sequence")

_UnionTypeExtension_Sequence_name = (Core.FieldName "name")

_UnionTypeExtension_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_UnionTypeExtension_Sequence_unionMemberTypes = (Core.FieldName "unionMemberTypes")

data UnionTypeExtension_Sequence2 
  = UnionTypeExtension_Sequence2 {
    unionTypeExtension_Sequence2Name :: Name,
    unionTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_UnionTypeExtension_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.UnionTypeExtension.Sequence2")

_UnionTypeExtension_Sequence2_name = (Core.FieldName "name")

_UnionTypeExtension_Sequence2_directives = (Core.FieldName "directives")

data EnumTypeDefinition 
  = EnumTypeDefinitionSequence EnumTypeDefinition_Sequence
  | EnumTypeDefinitionSequence2 EnumTypeDefinition_Sequence2
  deriving (Eq, Ord, Read, Show)

_EnumTypeDefinition = (Core.Name "hydra/ext/graphql/syntax.EnumTypeDefinition")

_EnumTypeDefinition_sequence = (Core.FieldName "sequence")

_EnumTypeDefinition_sequence2 = (Core.FieldName "sequence2")

data EnumTypeDefinition_Sequence 
  = EnumTypeDefinition_Sequence {
    enumTypeDefinition_SequenceOptionDescription :: (Maybe Description),
    enumTypeDefinition_SequenceName :: Name,
    enumTypeDefinition_SequenceOptionDirectives :: (Maybe Directives),
    enumTypeDefinition_SequenceEnumValuesDefinition :: EnumValuesDefinition}
  deriving (Eq, Ord, Read, Show)

_EnumTypeDefinition_Sequence = (Core.Name "hydra/ext/graphql/syntax.EnumTypeDefinition.Sequence")

_EnumTypeDefinition_Sequence_optionDescription = (Core.FieldName "optionDescription")

_EnumTypeDefinition_Sequence_name = (Core.FieldName "name")

_EnumTypeDefinition_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_EnumTypeDefinition_Sequence_enumValuesDefinition = (Core.FieldName "enumValuesDefinition")

data EnumTypeDefinition_Sequence2 
  = EnumTypeDefinition_Sequence2 {
    enumTypeDefinition_Sequence2OptionDescription :: (Maybe Description),
    enumTypeDefinition_Sequence2OptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_EnumTypeDefinition_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.EnumTypeDefinition.Sequence2")

_EnumTypeDefinition_Sequence2_optionDescription = (Core.FieldName "optionDescription")

_EnumTypeDefinition_Sequence2_optionDirectives = (Core.FieldName "optionDirectives")

data EnumValuesDefinition 
  = EnumValuesDefinition {
    enumValuesDefinitionStarEnumValueDefinition :: [EnumValueDefinition]}
  deriving (Eq, Ord, Read, Show)

_EnumValuesDefinition = (Core.Name "hydra/ext/graphql/syntax.EnumValuesDefinition")

_EnumValuesDefinition_starEnumValueDefinition = (Core.FieldName "starEnumValueDefinition")

data EnumValueDefinition 
  = EnumValueDefinition {
    enumValueDefinitionOptionDescription :: (Maybe Description),
    enumValueDefinitionEnumValue :: EnumValue,
    enumValueDefinitionOptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_EnumValueDefinition = (Core.Name "hydra/ext/graphql/syntax.EnumValueDefinition")

_EnumValueDefinition_optionDescription = (Core.FieldName "optionDescription")

_EnumValueDefinition_enumValue = (Core.FieldName "enumValue")

_EnumValueDefinition_optionDirectives = (Core.FieldName "optionDirectives")

data EnumTypeExtension 
  = EnumTypeExtensionSequence EnumTypeExtension_Sequence
  | EnumTypeExtensionSequence2 EnumTypeExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_EnumTypeExtension = (Core.Name "hydra/ext/graphql/syntax.EnumTypeExtension")

_EnumTypeExtension_sequence = (Core.FieldName "sequence")

_EnumTypeExtension_sequence2 = (Core.FieldName "sequence2")

data EnumTypeExtension_Sequence 
  = EnumTypeExtension_Sequence {
    enumTypeExtension_SequenceName :: Name,
    enumTypeExtension_SequenceOptionDirectives :: (Maybe Directives),
    enumTypeExtension_SequenceEnumValuesDefinition :: EnumValuesDefinition}
  deriving (Eq, Ord, Read, Show)

_EnumTypeExtension_Sequence = (Core.Name "hydra/ext/graphql/syntax.EnumTypeExtension.Sequence")

_EnumTypeExtension_Sequence_name = (Core.FieldName "name")

_EnumTypeExtension_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_EnumTypeExtension_Sequence_enumValuesDefinition = (Core.FieldName "enumValuesDefinition")

data EnumTypeExtension_Sequence2 
  = EnumTypeExtension_Sequence2 {
    enumTypeExtension_Sequence2Name :: Name,
    enumTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_EnumTypeExtension_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.EnumTypeExtension.Sequence2")

_EnumTypeExtension_Sequence2_name = (Core.FieldName "name")

_EnumTypeExtension_Sequence2_directives = (Core.FieldName "directives")

data InputObjectTypeDefinition 
  = InputObjectTypeDefinitionSequence InputObjectTypeDefinition_Sequence
  | InputObjectTypeDefinitionSequence2 InputObjectTypeDefinition_Sequence2
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeDefinition = (Core.Name "hydra/ext/graphql/syntax.InputObjectTypeDefinition")

_InputObjectTypeDefinition_sequence = (Core.FieldName "sequence")

_InputObjectTypeDefinition_sequence2 = (Core.FieldName "sequence2")

data InputObjectTypeDefinition_Sequence 
  = InputObjectTypeDefinition_Sequence {
    inputObjectTypeDefinition_SequenceOptionDescription :: (Maybe Description),
    inputObjectTypeDefinition_SequenceName :: Name,
    inputObjectTypeDefinition_SequenceOptionDirectives :: (Maybe Directives),
    inputObjectTypeDefinition_SequenceInputFieldsDefinition :: InputFieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeDefinition_Sequence = (Core.Name "hydra/ext/graphql/syntax.InputObjectTypeDefinition.Sequence")

_InputObjectTypeDefinition_Sequence_optionDescription = (Core.FieldName "optionDescription")

_InputObjectTypeDefinition_Sequence_name = (Core.FieldName "name")

_InputObjectTypeDefinition_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_InputObjectTypeDefinition_Sequence_inputFieldsDefinition = (Core.FieldName "inputFieldsDefinition")

data InputObjectTypeDefinition_Sequence2 
  = InputObjectTypeDefinition_Sequence2 {
    inputObjectTypeDefinition_Sequence2OptionDescription :: (Maybe Description),
    inputObjectTypeDefinition_Sequence2Name :: Name,
    inputObjectTypeDefinition_Sequence2OptionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeDefinition_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.InputObjectTypeDefinition.Sequence2")

_InputObjectTypeDefinition_Sequence2_optionDescription = (Core.FieldName "optionDescription")

_InputObjectTypeDefinition_Sequence2_name = (Core.FieldName "name")

_InputObjectTypeDefinition_Sequence2_optionDirectives = (Core.FieldName "optionDirectives")

data InputFieldsDefinition 
  = InputFieldsDefinition {
    inputFieldsDefinitionStarInputValueDefinition :: [InputValueDefinition]}
  deriving (Eq, Ord, Read, Show)

_InputFieldsDefinition = (Core.Name "hydra/ext/graphql/syntax.InputFieldsDefinition")

_InputFieldsDefinition_starInputValueDefinition = (Core.FieldName "starInputValueDefinition")

data InputObjectTypeExtension 
  = InputObjectTypeExtensionSequence InputObjectTypeExtension_Sequence
  | InputObjectTypeExtensionSequence2 InputObjectTypeExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeExtension = (Core.Name "hydra/ext/graphql/syntax.InputObjectTypeExtension")

_InputObjectTypeExtension_sequence = (Core.FieldName "sequence")

_InputObjectTypeExtension_sequence2 = (Core.FieldName "sequence2")

data InputObjectTypeExtension_Sequence 
  = InputObjectTypeExtension_Sequence {
    inputObjectTypeExtension_SequenceName :: Name,
    inputObjectTypeExtension_SequenceOptionDirectives :: (Maybe Directives),
    inputObjectTypeExtension_SequenceInputFieldsDefinition :: InputFieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeExtension_Sequence = (Core.Name "hydra/ext/graphql/syntax.InputObjectTypeExtension.Sequence")

_InputObjectTypeExtension_Sequence_name = (Core.FieldName "name")

_InputObjectTypeExtension_Sequence_optionDirectives = (Core.FieldName "optionDirectives")

_InputObjectTypeExtension_Sequence_inputFieldsDefinition = (Core.FieldName "inputFieldsDefinition")

data InputObjectTypeExtension_Sequence2 
  = InputObjectTypeExtension_Sequence2 {
    inputObjectTypeExtension_Sequence2Name :: Name,
    inputObjectTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeExtension_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.InputObjectTypeExtension.Sequence2")

_InputObjectTypeExtension_Sequence2_name = (Core.FieldName "name")

_InputObjectTypeExtension_Sequence2_directives = (Core.FieldName "directives")

data DirectiveDefinition 
  = DirectiveDefinition {
    directiveDefinitionOptionDescription :: (Maybe Description),
    directiveDefinitionName :: Name,
    directiveDefinitionOptionArgumentsDefinition :: (Maybe ArgumentsDefinition),
    directiveDefinitionOptionRepeatable :: (Maybe ()),
    directiveDefinitionDirectiveLocations :: DirectiveLocations}
  deriving (Eq, Ord, Read, Show)

_DirectiveDefinition = (Core.Name "hydra/ext/graphql/syntax.DirectiveDefinition")

_DirectiveDefinition_optionDescription = (Core.FieldName "optionDescription")

_DirectiveDefinition_name = (Core.FieldName "name")

_DirectiveDefinition_optionArgumentsDefinition = (Core.FieldName "optionArgumentsDefinition")

_DirectiveDefinition_optionRepeatable = (Core.FieldName "optionRepeatable")

_DirectiveDefinition_directiveLocations = (Core.FieldName "directiveLocations")

data DirectiveLocations 
  = DirectiveLocationsSequence DirectiveLocations_Sequence
  | DirectiveLocationsSequence2 DirectiveLocations_Sequence2
  deriving (Eq, Ord, Read, Show)

_DirectiveLocations = (Core.Name "hydra/ext/graphql/syntax.DirectiveLocations")

_DirectiveLocations_sequence = (Core.FieldName "sequence")

_DirectiveLocations_sequence2 = (Core.FieldName "sequence2")

data DirectiveLocations_Sequence 
  = DirectiveLocations_Sequence {
    directiveLocations_SequenceDirectiveLocations :: DirectiveLocations,
    directiveLocations_SequenceDirectiveLocation :: DirectiveLocation}
  deriving (Eq, Ord, Read, Show)

_DirectiveLocations_Sequence = (Core.Name "hydra/ext/graphql/syntax.DirectiveLocations.Sequence")

_DirectiveLocations_Sequence_directiveLocations = (Core.FieldName "directiveLocations")

_DirectiveLocations_Sequence_directiveLocation = (Core.FieldName "directiveLocation")

data DirectiveLocations_Sequence2 
  = DirectiveLocations_Sequence2 {
    directiveLocations_Sequence2OptionOr :: (Maybe ()),
    directiveLocations_Sequence2DirectiveLocation :: DirectiveLocation}
  deriving (Eq, Ord, Read, Show)

_DirectiveLocations_Sequence2 = (Core.Name "hydra/ext/graphql/syntax.DirectiveLocations.Sequence2")

_DirectiveLocations_Sequence2_optionOr = (Core.FieldName "optionOr")

_DirectiveLocations_Sequence2_directiveLocation = (Core.FieldName "directiveLocation")

data DirectiveLocation 
  = DirectiveLocationExecutableDirectiveLocation ExecutableDirectiveLocation
  | DirectiveLocationTypeSystemDirectiveLocation TypeSystemDirectiveLocation
  deriving (Eq, Ord, Read, Show)

_DirectiveLocation = (Core.Name "hydra/ext/graphql/syntax.DirectiveLocation")

_DirectiveLocation_executableDirectiveLocation = (Core.FieldName "executableDirectiveLocation")

_DirectiveLocation_typeSystemDirectiveLocation = (Core.FieldName "typeSystemDirectiveLocation")

data ExecutableDirectiveLocation 
  = ExecutableDirectiveLocationQUERY 
  | ExecutableDirectiveLocationMUTATION 
  | ExecutableDirectiveLocationSUBSCRIPTION 
  | ExecutableDirectiveLocationFIELD 
  | ExecutableDirectiveLocationFRAGMENTLowbarDEFINITION 
  | ExecutableDirectiveLocationFRAGMENTLowbarSPREAD 
  | ExecutableDirectiveLocationINLINELowbarFRAGMENT 
  | ExecutableDirectiveLocationVARIABLELowbarDEFINITION 
  deriving (Eq, Ord, Read, Show)

_ExecutableDirectiveLocation = (Core.Name "hydra/ext/graphql/syntax.ExecutableDirectiveLocation")

_ExecutableDirectiveLocation_qUERY = (Core.FieldName "qUERY")

_ExecutableDirectiveLocation_mUTATION = (Core.FieldName "mUTATION")

_ExecutableDirectiveLocation_sUBSCRIPTION = (Core.FieldName "sUBSCRIPTION")

_ExecutableDirectiveLocation_fIELD = (Core.FieldName "fIELD")

_ExecutableDirectiveLocation_fRAGMENTLowbarDEFINITION = (Core.FieldName "fRAGMENTLowbarDEFINITION")

_ExecutableDirectiveLocation_fRAGMENTLowbarSPREAD = (Core.FieldName "fRAGMENTLowbarSPREAD")

_ExecutableDirectiveLocation_iNLINELowbarFRAGMENT = (Core.FieldName "iNLINELowbarFRAGMENT")

_ExecutableDirectiveLocation_vARIABLELowbarDEFINITION = (Core.FieldName "vARIABLELowbarDEFINITION")

data TypeSystemDirectiveLocation 
  = TypeSystemDirectiveLocationSCHEMA 
  | TypeSystemDirectiveLocationSCALAR 
  | TypeSystemDirectiveLocationOBJECT 
  | TypeSystemDirectiveLocationFIELDLowbarDEFINITION 
  | TypeSystemDirectiveLocationARGUMENTLowbarDEFINITION 
  | TypeSystemDirectiveLocationINTERFACE 
  | TypeSystemDirectiveLocationUNION 
  | TypeSystemDirectiveLocationENUM 
  | TypeSystemDirectiveLocationENUMLowbarVALUE 
  | TypeSystemDirectiveLocationINPUTLowbarOBJECT 
  | TypeSystemDirectiveLocationINPUTLowbarFIELDLowbarDEFINITION 
  deriving (Eq, Ord, Read, Show)

_TypeSystemDirectiveLocation = (Core.Name "hydra/ext/graphql/syntax.TypeSystemDirectiveLocation")

_TypeSystemDirectiveLocation_sCHEMA = (Core.FieldName "sCHEMA")

_TypeSystemDirectiveLocation_sCALAR = (Core.FieldName "sCALAR")

_TypeSystemDirectiveLocation_oBJECT = (Core.FieldName "oBJECT")

_TypeSystemDirectiveLocation_fIELDLowbarDEFINITION = (Core.FieldName "fIELDLowbarDEFINITION")

_TypeSystemDirectiveLocation_aRGUMENTLowbarDEFINITION = (Core.FieldName "aRGUMENTLowbarDEFINITION")

_TypeSystemDirectiveLocation_iNTERFACE = (Core.FieldName "iNTERFACE")

_TypeSystemDirectiveLocation_uNION = (Core.FieldName "uNION")

_TypeSystemDirectiveLocation_eNUM = (Core.FieldName "eNUM")

_TypeSystemDirectiveLocation_eNUMLowbarVALUE = (Core.FieldName "eNUMLowbarVALUE")

_TypeSystemDirectiveLocation_iNPUTLowbarOBJECT = (Core.FieldName "iNPUTLowbarOBJECT")

_TypeSystemDirectiveLocation_iNPUTLowbarFIELDLowbarDEFINITION = (Core.FieldName "iNPUTLowbarFIELDLowbarDEFINITION")