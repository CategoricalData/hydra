-- Note: this is an automatically generated file. Do not edit.

-- | A GraphQL model. Based on the (extended) BNF at:
-- |   https://spec.graphql.org/draft/#sec-Appendix-Grammar-Summary

module Hydra.Ext.Org.Graphql.Syntax where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

newtype Name = 
  Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra.ext.org.graphql.syntax.Name")

newtype IntValue = 
  IntValue {
    unIntValue :: String}
  deriving (Eq, Ord, Read, Show)

_IntValue = (Core.Name "hydra.ext.org.graphql.syntax.IntValue")

newtype FloatValue = 
  FloatValue {
    unFloatValue :: String}
  deriving (Eq, Ord, Read, Show)

_FloatValue = (Core.Name "hydra.ext.org.graphql.syntax.FloatValue")

newtype StringValue = 
  StringValue {
    unStringValue :: String}
  deriving (Eq, Ord, Read, Show)

_StringValue = (Core.Name "hydra.ext.org.graphql.syntax.StringValue")

newtype Document = 
  Document {
    unDocument :: [Definition]}
  deriving (Eq, Ord, Read, Show)

_Document = (Core.Name "hydra.ext.org.graphql.syntax.Document")

data Definition = 
  DefinitionExecutable ExecutableDefinition |
  DefinitionTypeSystem TypeSystemDefinitionOrExtension
  deriving (Eq, Ord, Read, Show)

_Definition = (Core.Name "hydra.ext.org.graphql.syntax.Definition")

_Definition_executable = (Core.Name "executable")

_Definition_typeSystem = (Core.Name "typeSystem")

newtype ExecutableDocument = 
  ExecutableDocument {
    unExecutableDocument :: [ExecutableDefinition]}
  deriving (Eq, Ord, Read, Show)

_ExecutableDocument = (Core.Name "hydra.ext.org.graphql.syntax.ExecutableDocument")

data ExecutableDefinition = 
  ExecutableDefinitionOperation OperationDefinition |
  ExecutableDefinitionFragment FragmentDefinition
  deriving (Eq, Ord, Read, Show)

_ExecutableDefinition = (Core.Name "hydra.ext.org.graphql.syntax.ExecutableDefinition")

_ExecutableDefinition_operation = (Core.Name "operation")

_ExecutableDefinition_fragment = (Core.Name "fragment")

data OperationDefinition = 
  OperationDefinitionSequence OperationDefinition_Sequence |
  OperationDefinitionSelectionSet SelectionSet
  deriving (Eq, Ord, Read, Show)

_OperationDefinition = (Core.Name "hydra.ext.org.graphql.syntax.OperationDefinition")

_OperationDefinition_sequence = (Core.Name "sequence")

_OperationDefinition_SelectionSet = (Core.Name "SelectionSet")

data OperationDefinition_Sequence = 
  OperationDefinition_Sequence {
    operationDefinition_SequenceOperationType :: OperationType,
    operationDefinition_SequenceName :: (Maybe Name),
    operationDefinition_SequenceVariablesDefinition :: (Maybe VariablesDefinition),
    operationDefinition_SequenceDirectives :: (Maybe Directives),
    operationDefinition_SequenceSelectionSet :: SelectionSet}
  deriving (Eq, Ord, Read, Show)

_OperationDefinition_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.OperationDefinition_Sequence")

_OperationDefinition_Sequence_OperationType = (Core.Name "OperationType")

_OperationDefinition_Sequence_Name = (Core.Name "Name")

_OperationDefinition_Sequence_VariablesDefinition = (Core.Name "VariablesDefinition")

_OperationDefinition_Sequence_Directives = (Core.Name "Directives")

_OperationDefinition_Sequence_SelectionSet = (Core.Name "SelectionSet")

data OperationType = 
  OperationTypeQuery  |
  OperationTypeMutation  |
  OperationTypeSubscription 
  deriving (Eq, Ord, Read, Show)

_OperationType = (Core.Name "hydra.ext.org.graphql.syntax.OperationType")

_OperationType_Query = (Core.Name "Query")

_OperationType_Mutation = (Core.Name "Mutation")

_OperationType_Subscription = (Core.Name "Subscription")

newtype SelectionSet = 
  SelectionSet {
    unSelectionSet :: [Selection]}
  deriving (Eq, Ord, Read, Show)

_SelectionSet = (Core.Name "hydra.ext.org.graphql.syntax.SelectionSet")

data Selection = 
  SelectionField Field |
  SelectionFragmentSpread FragmentSpread |
  SelectionInlineFragment InlineFragment
  deriving (Eq, Ord, Read, Show)

_Selection = (Core.Name "hydra.ext.org.graphql.syntax.Selection")

_Selection_Field = (Core.Name "Field")

_Selection_FragmentSpread = (Core.Name "FragmentSpread")

_Selection_InlineFragment = (Core.Name "InlineFragment")

data Field = 
  Field {
    fieldAlias :: (Maybe Alias),
    fieldName :: Name,
    fieldArguments :: (Maybe Arguments),
    fieldDirectives :: (Maybe Directives),
    fieldSelectionSet :: (Maybe SelectionSet)}
  deriving (Eq, Ord, Read, Show)

_Field = (Core.Name "hydra.ext.org.graphql.syntax.Field")

_Field_Alias = (Core.Name "Alias")

_Field_Name = (Core.Name "Name")

_Field_Arguments = (Core.Name "Arguments")

_Field_Directives = (Core.Name "Directives")

_Field_SelectionSet = (Core.Name "SelectionSet")

data Alias = 
  AliasName Name |
  AliasColon 
  deriving (Eq, Ord, Read, Show)

_Alias = (Core.Name "hydra.ext.org.graphql.syntax.Alias")

_Alias_Name = (Core.Name "Name")

_Alias_Colon = (Core.Name "Colon")

newtype Arguments = 
  Arguments {
    unArguments :: [Argument]}
  deriving (Eq, Ord, Read, Show)

_Arguments = (Core.Name "hydra.ext.org.graphql.syntax.Arguments")

data Argument = 
  Argument {
    argumentName :: Name,
    argumentValue :: Value}
  deriving (Eq, Ord, Read, Show)

_Argument = (Core.Name "hydra.ext.org.graphql.syntax.Argument")

_Argument_Name = (Core.Name "Name")

_Argument_Value = (Core.Name "Value")

data FragmentSpread = 
  FragmentSpread {
    fragmentSpreadFragmentName :: FragmentName,
    fragmentSpreadDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_FragmentSpread = (Core.Name "hydra.ext.org.graphql.syntax.FragmentSpread")

_FragmentSpread_FragmentName = (Core.Name "FragmentName")

_FragmentSpread_Directives = (Core.Name "Directives")

data InlineFragment = 
  InlineFragment {
    inlineFragmentTypeCondition :: (Maybe TypeCondition),
    inlineFragmentDirectives :: (Maybe Directives),
    inlineFragmentSelectionSet :: SelectionSet}
  deriving (Eq, Ord, Read, Show)

_InlineFragment = (Core.Name "hydra.ext.org.graphql.syntax.InlineFragment")

_InlineFragment_TypeCondition = (Core.Name "TypeCondition")

_InlineFragment_Directives = (Core.Name "Directives")

_InlineFragment_SelectionSet = (Core.Name "SelectionSet")

data FragmentDefinition = 
  FragmentDefinition {
    fragmentDefinitionFragmentName :: FragmentName,
    fragmentDefinitionTypeCondition :: TypeCondition,
    fragmentDefinitionDirectives :: (Maybe Directives),
    fragmentDefinitionSelectionSet :: SelectionSet}
  deriving (Eq, Ord, Read, Show)

_FragmentDefinition = (Core.Name "hydra.ext.org.graphql.syntax.FragmentDefinition")

_FragmentDefinition_FragmentName = (Core.Name "FragmentName")

_FragmentDefinition_TypeCondition = (Core.Name "TypeCondition")

_FragmentDefinition_Directives = (Core.Name "Directives")

_FragmentDefinition_SelectionSet = (Core.Name "SelectionSet")

newtype FragmentName = 
  FragmentName {
    unFragmentName :: Name}
  deriving (Eq, Ord, Read, Show)

_FragmentName = (Core.Name "hydra.ext.org.graphql.syntax.FragmentName")

data TypeCondition = 
  TypeConditionOn  |
  TypeConditionNamedType NamedType
  deriving (Eq, Ord, Read, Show)

_TypeCondition = (Core.Name "hydra.ext.org.graphql.syntax.TypeCondition")

_TypeCondition_On = (Core.Name "On")

_TypeCondition_NamedType = (Core.Name "NamedType")

data Value = 
  ValueVariable Variable |
  ValueInt IntValue |
  ValueFloat FloatValue |
  ValueString StringValue |
  ValueBoolean BooleanValue |
  ValueNull NullValue |
  ValueEnum EnumValue |
  ValueList ListValue |
  ValueObject ObjectValue
  deriving (Eq, Ord, Read, Show)

_Value = (Core.Name "hydra.ext.org.graphql.syntax.Value")

_Value_Variable = (Core.Name "Variable")

_Value_int = (Core.Name "int")

_Value_float = (Core.Name "float")

_Value_string = (Core.Name "string")

_Value_boolean = (Core.Name "boolean")

_Value_null = (Core.Name "null")

_Value_enum = (Core.Name "enum")

_Value_list = (Core.Name "list")

_Value_object = (Core.Name "object")

data BooleanValue = 
  BooleanValueTrue  |
  BooleanValueFalse 
  deriving (Eq, Ord, Read, Show)

_BooleanValue = (Core.Name "hydra.ext.org.graphql.syntax.BooleanValue")

_BooleanValue_True = (Core.Name "True")

_BooleanValue_False = (Core.Name "False")

newtype NullValue = 
  NullValue {
    unNullValue :: ()}
  deriving (Eq, Ord, Read, Show)

_NullValue = (Core.Name "hydra.ext.org.graphql.syntax.NullValue")

newtype EnumValue = 
  EnumValue {
    unEnumValue :: Name}
  deriving (Eq, Ord, Read, Show)

_EnumValue = (Core.Name "hydra.ext.org.graphql.syntax.EnumValue")

data ListValue = 
  ListValueSequence ListValue_Sequence |
  ListValueSequence2 [Value]
  deriving (Eq, Ord, Read, Show)

_ListValue = (Core.Name "hydra.ext.org.graphql.syntax.ListValue")

_ListValue_sequence = (Core.Name "sequence")

_ListValue_sequence2 = (Core.Name "sequence2")

data ListValue_Sequence = 
  ListValue_Sequence {}
  deriving (Eq, Ord, Read, Show)

_ListValue_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.ListValue_Sequence")

data ObjectValue = 
  ObjectValueSequence ObjectValue_Sequence |
  ObjectValueSequence2 [ObjectField]
  deriving (Eq, Ord, Read, Show)

_ObjectValue = (Core.Name "hydra.ext.org.graphql.syntax.ObjectValue")

_ObjectValue_sequence = (Core.Name "sequence")

_ObjectValue_sequence2 = (Core.Name "sequence2")

data ObjectValue_Sequence = 
  ObjectValue_Sequence {}
  deriving (Eq, Ord, Read, Show)

_ObjectValue_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.ObjectValue_Sequence")

data ObjectField = 
  ObjectField {
    objectFieldName :: Name,
    objectFieldValue :: Value}
  deriving (Eq, Ord, Read, Show)

_ObjectField = (Core.Name "hydra.ext.org.graphql.syntax.ObjectField")

_ObjectField_Name = (Core.Name "Name")

_ObjectField_Value = (Core.Name "Value")

data VariablesDefinition = 
  VariablesDefinition {
    variablesDefinitionVariable :: Variable,
    variablesDefinitionType :: Type,
    variablesDefinitionDefaultValue :: (Maybe DefaultValue),
    variablesDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_VariablesDefinition = (Core.Name "hydra.ext.org.graphql.syntax.VariablesDefinition")

_VariablesDefinition_Variable = (Core.Name "Variable")

_VariablesDefinition_Type = (Core.Name "Type")

_VariablesDefinition_DefaultValue = (Core.Name "DefaultValue")

_VariablesDefinition_Directives = (Core.Name "Directives")

newtype Variable = 
  Variable {
    unVariable :: Name}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra.ext.org.graphql.syntax.Variable")

newtype DefaultValue = 
  DefaultValue {
    unDefaultValue :: Value}
  deriving (Eq, Ord, Read, Show)

_DefaultValue = (Core.Name "hydra.ext.org.graphql.syntax.DefaultValue")

data Type = 
  TypeNamed NamedType |
  TypeList ListType |
  TypeNonNull NonNullType
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra.ext.org.graphql.syntax.Type")

_Type_named = (Core.Name "named")

_Type_list = (Core.Name "list")

_Type_nonNull = (Core.Name "nonNull")

newtype NamedType = 
  NamedType {
    unNamedType :: Name}
  deriving (Eq, Ord, Read, Show)

_NamedType = (Core.Name "hydra.ext.org.graphql.syntax.NamedType")

newtype ListType = 
  ListType {
    unListType :: Type}
  deriving (Eq, Ord, Read, Show)

_ListType = (Core.Name "hydra.ext.org.graphql.syntax.ListType")

data NonNullType = 
  NonNullTypeNamed NamedType |
  NonNullTypeList ListType
  deriving (Eq, Ord, Read, Show)

_NonNullType = (Core.Name "hydra.ext.org.graphql.syntax.NonNullType")

_NonNullType_named = (Core.Name "named")

_NonNullType_list = (Core.Name "list")

newtype Directives = 
  Directives {
    unDirectives :: [Directive]}
  deriving (Eq, Ord, Read, Show)

_Directives = (Core.Name "hydra.ext.org.graphql.syntax.Directives")

data Directive = 
  Directive {
    directiveName :: Name,
    directiveArguments :: (Maybe Arguments)}
  deriving (Eq, Ord, Read, Show)

_Directive = (Core.Name "hydra.ext.org.graphql.syntax.Directive")

_Directive_Name = (Core.Name "Name")

_Directive_Arguments = (Core.Name "Arguments")

newtype TypeSystemDocment = 
  TypeSystemDocment {
    unTypeSystemDocment :: [TypeSystemDefinition]}
  deriving (Eq, Ord, Read, Show)

_TypeSystemDocment = (Core.Name "hydra.ext.org.graphql.syntax.TypeSystemDocment")

data TypeSystemDefinition = 
  TypeSystemDefinitionSchema SchemaDefinition |
  TypeSystemDefinitionType TypeDefinition |
  TypeSystemDefinitionDirective DirectiveDefinition
  deriving (Eq, Ord, Read, Show)

_TypeSystemDefinition = (Core.Name "hydra.ext.org.graphql.syntax.TypeSystemDefinition")

_TypeSystemDefinition_schema = (Core.Name "schema")

_TypeSystemDefinition_type = (Core.Name "type")

_TypeSystemDefinition_directive = (Core.Name "directive")

newtype TypeSystemExtensionDocument = 
  TypeSystemExtensionDocument {
    unTypeSystemExtensionDocument :: [TypeSystemDefinitionOrExtension]}
  deriving (Eq, Ord, Read, Show)

_TypeSystemExtensionDocument = (Core.Name "hydra.ext.org.graphql.syntax.TypeSystemExtensionDocument")

data TypeSystemDefinitionOrExtension = 
  TypeSystemDefinitionOrExtensionDefinition TypeSystemDefinition |
  TypeSystemDefinitionOrExtensionExtension TypeSystemExtension
  deriving (Eq, Ord, Read, Show)

_TypeSystemDefinitionOrExtension = (Core.Name "hydra.ext.org.graphql.syntax.TypeSystemDefinitionOrExtension")

_TypeSystemDefinitionOrExtension_definition = (Core.Name "definition")

_TypeSystemDefinitionOrExtension_extension = (Core.Name "extension")

data TypeSystemExtension = 
  TypeSystemExtensionSchema SchemaExtension |
  TypeSystemExtensionType TypeExtension
  deriving (Eq, Ord, Read, Show)

_TypeSystemExtension = (Core.Name "hydra.ext.org.graphql.syntax.TypeSystemExtension")

_TypeSystemExtension_schema = (Core.Name "schema")

_TypeSystemExtension_type = (Core.Name "type")

data SchemaDefinition = 
  SchemaDefinition {
    schemaDefinitionDescription :: (Maybe Description),
    schemaDefinitionDirectives :: (Maybe Directives),
    schemaDefinitionRootOperationTypeDefinition :: RootOperationTypeDefinition}
  deriving (Eq, Ord, Read, Show)

_SchemaDefinition = (Core.Name "hydra.ext.org.graphql.syntax.SchemaDefinition")

_SchemaDefinition_Description = (Core.Name "Description")

_SchemaDefinition_Directives = (Core.Name "Directives")

_SchemaDefinition_RootOperationTypeDefinition = (Core.Name "RootOperationTypeDefinition")

data SchemaExtension = 
  SchemaExtensionSequence SchemaExtension_Sequence |
  SchemaExtensionSequence2 Directives
  deriving (Eq, Ord, Read, Show)

_SchemaExtension = (Core.Name "hydra.ext.org.graphql.syntax.SchemaExtension")

_SchemaExtension_sequence = (Core.Name "sequence")

_SchemaExtension_sequence2 = (Core.Name "sequence2")

data SchemaExtension_Sequence = 
  SchemaExtension_Sequence {
    schemaExtension_SequenceDirectives :: (Maybe Directives),
    schemaExtension_SequenceRootOperationTypeDefinition :: RootOperationTypeDefinition}
  deriving (Eq, Ord, Read, Show)

_SchemaExtension_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.SchemaExtension_Sequence")

_SchemaExtension_Sequence_Directives = (Core.Name "Directives")

_SchemaExtension_Sequence_RootOperationTypeDefinition = (Core.Name "RootOperationTypeDefinition")

data RootOperationTypeDefinition = 
  RootOperationTypeDefinition {
    rootOperationTypeDefinitionOperationType :: OperationType,
    rootOperationTypeDefinitionNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_RootOperationTypeDefinition = (Core.Name "hydra.ext.org.graphql.syntax.RootOperationTypeDefinition")

_RootOperationTypeDefinition_OperationType = (Core.Name "OperationType")

_RootOperationTypeDefinition_NamedType = (Core.Name "NamedType")

newtype Description = 
  Description {
    unDescription :: StringValue}
  deriving (Eq, Ord, Read, Show)

_Description = (Core.Name "hydra.ext.org.graphql.syntax.Description")

data TypeDefinition = 
  TypeDefinitionScalar ScalarTypeDefinition |
  TypeDefinitionObject ObjectTypeDefinition |
  TypeDefinitionInterface InterfaceTypeDefinition |
  TypeDefinitionUnion UnionTypeDefinition |
  TypeDefinitionEnum EnumTypeDefinition |
  TypeDefinitionInputObject InputObjectTypeDefinition
  deriving (Eq, Ord, Read, Show)

_TypeDefinition = (Core.Name "hydra.ext.org.graphql.syntax.TypeDefinition")

_TypeDefinition_scalar = (Core.Name "scalar")

_TypeDefinition_object = (Core.Name "object")

_TypeDefinition_interface = (Core.Name "interface")

_TypeDefinition_union = (Core.Name "union")

_TypeDefinition_enum = (Core.Name "enum")

_TypeDefinition_inputObject = (Core.Name "inputObject")

data TypeExtension = 
  TypeExtensionScalar ScalarTypeExtension |
  TypeExtensionObject ObjectTypeExtension |
  TypeExtensionInterface InterfaceTypeExtension |
  TypeExtensionUnion UnionTypeExtension |
  TypeExtensionEnum EnumTypeExtension |
  TypeExtensionInputObject InputObjectTypeExtension
  deriving (Eq, Ord, Read, Show)

_TypeExtension = (Core.Name "hydra.ext.org.graphql.syntax.TypeExtension")

_TypeExtension_scalar = (Core.Name "scalar")

_TypeExtension_object = (Core.Name "object")

_TypeExtension_interface = (Core.Name "interface")

_TypeExtension_union = (Core.Name "union")

_TypeExtension_enum = (Core.Name "enum")

_TypeExtension_inputObject = (Core.Name "inputObject")

data ScalarTypeDefinition = 
  ScalarTypeDefinition {
    scalarTypeDefinitionDescription :: (Maybe Description),
    scalarTypeDefinitionName :: Name,
    scalarTypeDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_ScalarTypeDefinition = (Core.Name "hydra.ext.org.graphql.syntax.ScalarTypeDefinition")

_ScalarTypeDefinition_Description = (Core.Name "Description")

_ScalarTypeDefinition_Name = (Core.Name "Name")

_ScalarTypeDefinition_Directives = (Core.Name "Directives")

data ScalarTypeExtension = 
  ScalarTypeExtension {
    scalarTypeExtensionName :: Name,
    scalarTypeExtensionDirectives :: Directives}
  deriving (Eq, Ord, Read, Show)

_ScalarTypeExtension = (Core.Name "hydra.ext.org.graphql.syntax.ScalarTypeExtension")

_ScalarTypeExtension_Name = (Core.Name "Name")

_ScalarTypeExtension_Directives = (Core.Name "Directives")

data ObjectTypeDefinition = 
  ObjectTypeDefinition {
    objectTypeDefinitionDescription :: (Maybe Description),
    objectTypeDefinitionName :: Name,
    objectTypeDefinitionImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeDefinitionDirectives :: (Maybe Directives),
    objectTypeDefinitionFieldsDefinition :: (Maybe FieldsDefinition)}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeDefinition = (Core.Name "hydra.ext.org.graphql.syntax.ObjectTypeDefinition")

_ObjectTypeDefinition_Description = (Core.Name "Description")

_ObjectTypeDefinition_Name = (Core.Name "Name")

_ObjectTypeDefinition_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

_ObjectTypeDefinition_Directives = (Core.Name "Directives")

_ObjectTypeDefinition_FieldsDefinition = (Core.Name "FieldsDefinition")

data ObjectTypeExtension = 
  ObjectTypeExtensionSequence ObjectTypeExtension_Sequence |
  ObjectTypeExtensionSequence2 ObjectTypeExtension_Sequence2 |
  ObjectTypeExtensionSequence3 ObjectTypeExtension_Sequence3
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension = (Core.Name "hydra.ext.org.graphql.syntax.ObjectTypeExtension")

_ObjectTypeExtension_sequence = (Core.Name "sequence")

_ObjectTypeExtension_sequence2 = (Core.Name "sequence2")

_ObjectTypeExtension_sequence3 = (Core.Name "sequence3")

data ObjectTypeExtension_Sequence = 
  ObjectTypeExtension_Sequence {
    objectTypeExtension_SequenceName :: Name,
    objectTypeExtension_SequenceImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeExtension_SequenceDirectives :: (Maybe Directives),
    objectTypeExtension_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.ObjectTypeExtension_Sequence")

_ObjectTypeExtension_Sequence_Name = (Core.Name "Name")

_ObjectTypeExtension_Sequence_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

_ObjectTypeExtension_Sequence_Directives = (Core.Name "Directives")

_ObjectTypeExtension_Sequence_FieldsDefinition = (Core.Name "FieldsDefinition")

data ObjectTypeExtension_Sequence2 = 
  ObjectTypeExtension_Sequence2 {
    objectTypeExtension_Sequence2Name :: Name,
    objectTypeExtension_Sequence2ImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeExtension_Sequence2Directives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.ObjectTypeExtension_Sequence2")

_ObjectTypeExtension_Sequence2_Name = (Core.Name "Name")

_ObjectTypeExtension_Sequence2_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

_ObjectTypeExtension_Sequence2_Directives = (Core.Name "Directives")

data ObjectTypeExtension_Sequence3 = 
  ObjectTypeExtension_Sequence3 {
    objectTypeExtension_Sequence3Name :: Name,
    objectTypeExtension_Sequence3ImplementsInterfaces :: ImplementsInterfaces}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension_Sequence3 = (Core.Name "hydra.ext.org.graphql.syntax.ObjectTypeExtension_Sequence3")

_ObjectTypeExtension_Sequence3_Name = (Core.Name "Name")

_ObjectTypeExtension_Sequence3_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

data ImplementsInterfaces = 
  ImplementsInterfacesSequence ImplementsInterfaces_Sequence |
  ImplementsInterfacesSequence2 ImplementsInterfaces_Sequence2
  deriving (Eq, Ord, Read, Show)

_ImplementsInterfaces = (Core.Name "hydra.ext.org.graphql.syntax.ImplementsInterfaces")

_ImplementsInterfaces_sequence = (Core.Name "sequence")

_ImplementsInterfaces_sequence2 = (Core.Name "sequence2")

data ImplementsInterfaces_Sequence = 
  ImplementsInterfaces_Sequence {
    implementsInterfaces_SequenceImplementsInterfaces :: ImplementsInterfaces,
    implementsInterfaces_SequenceNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_ImplementsInterfaces_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.ImplementsInterfaces_Sequence")

_ImplementsInterfaces_Sequence_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

_ImplementsInterfaces_Sequence_NamedType = (Core.Name "NamedType")

data ImplementsInterfaces_Sequence2 = 
  ImplementsInterfaces_Sequence2 {
    implementsInterfaces_Sequence2Amp :: (Maybe ()),
    implementsInterfaces_Sequence2NamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_ImplementsInterfaces_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.ImplementsInterfaces_Sequence2")

_ImplementsInterfaces_Sequence2_Amp = (Core.Name "Amp")

_ImplementsInterfaces_Sequence2_NamedType = (Core.Name "NamedType")

newtype FieldsDefinition = 
  FieldsDefinition {
    unFieldsDefinition :: [FieldDefinition]}
  deriving (Eq, Ord, Read, Show)

_FieldsDefinition = (Core.Name "hydra.ext.org.graphql.syntax.FieldsDefinition")

data FieldDefinition = 
  FieldDefinition {
    fieldDefinitionDescription :: (Maybe Description),
    fieldDefinitionName :: Name,
    fieldDefinitionArgumentsDefinition :: (Maybe ArgumentsDefinition),
    fieldDefinitionType :: Type,
    fieldDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_FieldDefinition = (Core.Name "hydra.ext.org.graphql.syntax.FieldDefinition")

_FieldDefinition_Description = (Core.Name "Description")

_FieldDefinition_Name = (Core.Name "Name")

_FieldDefinition_ArgumentsDefinition = (Core.Name "ArgumentsDefinition")

_FieldDefinition_Type = (Core.Name "Type")

_FieldDefinition_Directives = (Core.Name "Directives")

newtype ArgumentsDefinition = 
  ArgumentsDefinition {
    unArgumentsDefinition :: [InputValueDefinition]}
  deriving (Eq, Ord, Read, Show)

_ArgumentsDefinition = (Core.Name "hydra.ext.org.graphql.syntax.ArgumentsDefinition")

data InputValueDefinition = 
  InputValueDefinition {
    inputValueDefinitionDescription :: (Maybe Description),
    inputValueDefinitionName :: Name,
    inputValueDefinitionType :: Type,
    inputValueDefinitionDefaultValue :: (Maybe DefaultValue),
    inputValueDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_InputValueDefinition = (Core.Name "hydra.ext.org.graphql.syntax.InputValueDefinition")

_InputValueDefinition_Description = (Core.Name "Description")

_InputValueDefinition_Name = (Core.Name "Name")

_InputValueDefinition_Type = (Core.Name "Type")

_InputValueDefinition_DefaultValue = (Core.Name "DefaultValue")

_InputValueDefinition_Directives = (Core.Name "Directives")

data InterfaceTypeDefinition = 
  InterfaceTypeDefinitionSequence InterfaceTypeDefinition_Sequence |
  InterfaceTypeDefinitionSequence2 InterfaceTypeDefinition_Sequence2
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeDefinition = (Core.Name "hydra.ext.org.graphql.syntax.InterfaceTypeDefinition")

_InterfaceTypeDefinition_sequence = (Core.Name "sequence")

_InterfaceTypeDefinition_sequence2 = (Core.Name "sequence2")

data InterfaceTypeDefinition_Sequence = 
  InterfaceTypeDefinition_Sequence {
    interfaceTypeDefinition_SequenceDescription :: (Maybe Description),
    interfaceTypeDefinition_SequenceName :: Name,
    interfaceTypeDefinition_SequenceImplementsInterfaces :: (Maybe ImplementsInterfaces),
    interfaceTypeDefinition_SequenceDirectives :: (Maybe Directives),
    interfaceTypeDefinition_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeDefinition_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.InterfaceTypeDefinition_Sequence")

_InterfaceTypeDefinition_Sequence_Description = (Core.Name "Description")

_InterfaceTypeDefinition_Sequence_Name = (Core.Name "Name")

_InterfaceTypeDefinition_Sequence_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

_InterfaceTypeDefinition_Sequence_Directives = (Core.Name "Directives")

_InterfaceTypeDefinition_Sequence_FieldsDefinition = (Core.Name "FieldsDefinition")

data InterfaceTypeDefinition_Sequence2 = 
  InterfaceTypeDefinition_Sequence2 {
    interfaceTypeDefinition_Sequence2Description :: (Maybe Description),
    interfaceTypeDefinition_Sequence2Name :: Name,
    interfaceTypeDefinition_Sequence2ImplementsInterfaces :: ImplementsInterfaces,
    interfaceTypeDefinition_Sequence2Directives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeDefinition_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.InterfaceTypeDefinition_Sequence2")

_InterfaceTypeDefinition_Sequence2_Description = (Core.Name "Description")

_InterfaceTypeDefinition_Sequence2_Name = (Core.Name "Name")

_InterfaceTypeDefinition_Sequence2_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

_InterfaceTypeDefinition_Sequence2_Directives = (Core.Name "Directives")

data InterfaceTypeExtension = 
  InterfaceTypeExtensionSequence InterfaceTypeExtension_Sequence |
  InterfaceTypeExtensionSequence2 InterfaceTypeExtension_Sequence2 |
  InterfaceTypeExtensionSequence3 InterfaceTypeExtension_Sequence3
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension = (Core.Name "hydra.ext.org.graphql.syntax.InterfaceTypeExtension")

_InterfaceTypeExtension_sequence = (Core.Name "sequence")

_InterfaceTypeExtension_sequence2 = (Core.Name "sequence2")

_InterfaceTypeExtension_sequence3 = (Core.Name "sequence3")

data InterfaceTypeExtension_Sequence = 
  InterfaceTypeExtension_Sequence {
    interfaceTypeExtension_SequenceName :: Name,
    interfaceTypeExtension_SequenceImplementsInterfaces :: (Maybe ImplementsInterfaces),
    interfaceTypeExtension_SequenceDirectives :: (Maybe Directives),
    interfaceTypeExtension_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.InterfaceTypeExtension_Sequence")

_InterfaceTypeExtension_Sequence_Name = (Core.Name "Name")

_InterfaceTypeExtension_Sequence_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

_InterfaceTypeExtension_Sequence_Directives = (Core.Name "Directives")

_InterfaceTypeExtension_Sequence_FieldsDefinition = (Core.Name "FieldsDefinition")

data InterfaceTypeExtension_Sequence2 = 
  InterfaceTypeExtension_Sequence2 {
    interfaceTypeExtension_Sequence2Name :: Name,
    interfaceTypeExtension_Sequence2ImplementsInterfaces :: (Maybe ImplementsInterfaces),
    interfaceTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.InterfaceTypeExtension_Sequence2")

_InterfaceTypeExtension_Sequence2_Name = (Core.Name "Name")

_InterfaceTypeExtension_Sequence2_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

_InterfaceTypeExtension_Sequence2_Directives = (Core.Name "Directives")

data InterfaceTypeExtension_Sequence3 = 
  InterfaceTypeExtension_Sequence3 {
    interfaceTypeExtension_Sequence3Name :: Name,
    interfaceTypeExtension_Sequence3ImplementsInterfaces :: ImplementsInterfaces}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension_Sequence3 = (Core.Name "hydra.ext.org.graphql.syntax.InterfaceTypeExtension_Sequence3")

_InterfaceTypeExtension_Sequence3_Name = (Core.Name "Name")

_InterfaceTypeExtension_Sequence3_ImplementsInterfaces = (Core.Name "ImplementsInterfaces")

data UnionTypeDefinition = 
  UnionTypeDefinition {
    unionTypeDefinitionDescription :: (Maybe Description),
    unionTypeDefinitionName :: Name,
    unionTypeDefinitionDirectives :: (Maybe Directives),
    unionTypeDefinitionUnionMemberTypes :: (Maybe UnionMemberTypes)}
  deriving (Eq, Ord, Read, Show)

_UnionTypeDefinition = (Core.Name "hydra.ext.org.graphql.syntax.UnionTypeDefinition")

_UnionTypeDefinition_Description = (Core.Name "Description")

_UnionTypeDefinition_Name = (Core.Name "Name")

_UnionTypeDefinition_Directives = (Core.Name "Directives")

_UnionTypeDefinition_UnionMemberTypes = (Core.Name "UnionMemberTypes")

data UnionMemberTypes = 
  UnionMemberTypesSequence UnionMemberTypes_Sequence |
  UnionMemberTypesSequence2 UnionMemberTypes_Sequence2
  deriving (Eq, Ord, Read, Show)

_UnionMemberTypes = (Core.Name "hydra.ext.org.graphql.syntax.UnionMemberTypes")

_UnionMemberTypes_sequence = (Core.Name "sequence")

_UnionMemberTypes_sequence2 = (Core.Name "sequence2")

data UnionMemberTypes_Sequence = 
  UnionMemberTypes_Sequence {
    unionMemberTypes_SequenceUnionMemberTypes :: UnionMemberTypes,
    unionMemberTypes_SequenceNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_UnionMemberTypes_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.UnionMemberTypes_Sequence")

_UnionMemberTypes_Sequence_UnionMemberTypes = (Core.Name "UnionMemberTypes")

_UnionMemberTypes_Sequence_NamedType = (Core.Name "NamedType")

data UnionMemberTypes_Sequence2 = 
  UnionMemberTypes_Sequence2 {
    unionMemberTypes_Sequence2Or :: (Maybe ()),
    unionMemberTypes_Sequence2NamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_UnionMemberTypes_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.UnionMemberTypes_Sequence2")

_UnionMemberTypes_Sequence2_Or = (Core.Name "Or")

_UnionMemberTypes_Sequence2_NamedType = (Core.Name "NamedType")

data UnionTypeExtension = 
  UnionTypeExtensionSequence UnionTypeExtension_Sequence |
  UnionTypeExtensionSequence2 UnionTypeExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_UnionTypeExtension = (Core.Name "hydra.ext.org.graphql.syntax.UnionTypeExtension")

_UnionTypeExtension_sequence = (Core.Name "sequence")

_UnionTypeExtension_sequence2 = (Core.Name "sequence2")

data UnionTypeExtension_Sequence = 
  UnionTypeExtension_Sequence {
    unionTypeExtension_SequenceName :: Name,
    unionTypeExtension_SequenceDirectives :: (Maybe Directives),
    unionTypeExtension_SequenceUnionMemberTypes :: UnionMemberTypes}
  deriving (Eq, Ord, Read, Show)

_UnionTypeExtension_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.UnionTypeExtension_Sequence")

_UnionTypeExtension_Sequence_Name = (Core.Name "Name")

_UnionTypeExtension_Sequence_Directives = (Core.Name "Directives")

_UnionTypeExtension_Sequence_UnionMemberTypes = (Core.Name "UnionMemberTypes")

data UnionTypeExtension_Sequence2 = 
  UnionTypeExtension_Sequence2 {
    unionTypeExtension_Sequence2Name :: Name,
    unionTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_UnionTypeExtension_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.UnionTypeExtension_Sequence2")

_UnionTypeExtension_Sequence2_Name = (Core.Name "Name")

_UnionTypeExtension_Sequence2_Directives = (Core.Name "Directives")

data EnumTypeDefinition = 
  EnumTypeDefinition {
    enumTypeDefinitionDescription :: (Maybe Description),
    enumTypeDefinitionName :: Name,
    enumTypeDefinitionDirectives :: (Maybe Directives),
    enumTypeDefinitionEnumValuesDefinition :: (Maybe EnumValuesDefinition)}
  deriving (Eq, Ord, Read, Show)

_EnumTypeDefinition = (Core.Name "hydra.ext.org.graphql.syntax.EnumTypeDefinition")

_EnumTypeDefinition_Description = (Core.Name "Description")

_EnumTypeDefinition_Name = (Core.Name "Name")

_EnumTypeDefinition_Directives = (Core.Name "Directives")

_EnumTypeDefinition_EnumValuesDefinition = (Core.Name "EnumValuesDefinition")

newtype EnumValuesDefinition = 
  EnumValuesDefinition {
    unEnumValuesDefinition :: [EnumValueDefinition]}
  deriving (Eq, Ord, Read, Show)

_EnumValuesDefinition = (Core.Name "hydra.ext.org.graphql.syntax.EnumValuesDefinition")

data EnumValueDefinition = 
  EnumValueDefinition {
    enumValueDefinitionDescription :: (Maybe Description),
    enumValueDefinitionEnumValue :: EnumValue,
    enumValueDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_EnumValueDefinition = (Core.Name "hydra.ext.org.graphql.syntax.EnumValueDefinition")

_EnumValueDefinition_Description = (Core.Name "Description")

_EnumValueDefinition_EnumValue = (Core.Name "EnumValue")

_EnumValueDefinition_Directives = (Core.Name "Directives")

data EnumTypeExtension = 
  EnumTypeExtensionSequence EnumTypeExtension_Sequence |
  EnumTypeExtensionSequence2 EnumTypeExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_EnumTypeExtension = (Core.Name "hydra.ext.org.graphql.syntax.EnumTypeExtension")

_EnumTypeExtension_sequence = (Core.Name "sequence")

_EnumTypeExtension_sequence2 = (Core.Name "sequence2")

data EnumTypeExtension_Sequence = 
  EnumTypeExtension_Sequence {
    enumTypeExtension_SequenceName :: Name,
    enumTypeExtension_SequenceDirectives :: (Maybe Directives),
    enumTypeExtension_SequenceEnumValuesDefinition :: EnumValuesDefinition}
  deriving (Eq, Ord, Read, Show)

_EnumTypeExtension_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.EnumTypeExtension_Sequence")

_EnumTypeExtension_Sequence_Name = (Core.Name "Name")

_EnumTypeExtension_Sequence_Directives = (Core.Name "Directives")

_EnumTypeExtension_Sequence_EnumValuesDefinition = (Core.Name "EnumValuesDefinition")

data EnumTypeExtension_Sequence2 = 
  EnumTypeExtension_Sequence2 {
    enumTypeExtension_Sequence2Name :: Name,
    enumTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_EnumTypeExtension_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.EnumTypeExtension_Sequence2")

_EnumTypeExtension_Sequence2_Name = (Core.Name "Name")

_EnumTypeExtension_Sequence2_Directives = (Core.Name "Directives")

data InputObjectTypeDefinition = 
  InputObjectTypeDefinitionSequence InputObjectTypeDefinition_Sequence |
  InputObjectTypeDefinitionSequence2 InputObjectTypeDefinition_Sequence2
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeDefinition = (Core.Name "hydra.ext.org.graphql.syntax.InputObjectTypeDefinition")

_InputObjectTypeDefinition_sequence = (Core.Name "sequence")

_InputObjectTypeDefinition_sequence2 = (Core.Name "sequence2")

data InputObjectTypeDefinition_Sequence = 
  InputObjectTypeDefinition_Sequence {
    inputObjectTypeDefinition_SequenceDescription :: (Maybe Description),
    inputObjectTypeDefinition_SequenceName :: Name,
    inputObjectTypeDefinition_SequenceDirectives :: (Maybe Directives),
    inputObjectTypeDefinition_SequenceInputFieldsDefinition :: InputFieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeDefinition_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.InputObjectTypeDefinition_Sequence")

_InputObjectTypeDefinition_Sequence_Description = (Core.Name "Description")

_InputObjectTypeDefinition_Sequence_Name = (Core.Name "Name")

_InputObjectTypeDefinition_Sequence_Directives = (Core.Name "Directives")

_InputObjectTypeDefinition_Sequence_InputFieldsDefinition = (Core.Name "InputFieldsDefinition")

data InputObjectTypeDefinition_Sequence2 = 
  InputObjectTypeDefinition_Sequence2 {
    inputObjectTypeDefinition_Sequence2Description :: (Maybe Description),
    inputObjectTypeDefinition_Sequence2Name :: Name,
    inputObjectTypeDefinition_Sequence2Directives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeDefinition_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.InputObjectTypeDefinition_Sequence2")

_InputObjectTypeDefinition_Sequence2_Description = (Core.Name "Description")

_InputObjectTypeDefinition_Sequence2_Name = (Core.Name "Name")

_InputObjectTypeDefinition_Sequence2_Directives = (Core.Name "Directives")

newtype InputFieldsDefinition = 
  InputFieldsDefinition {
    unInputFieldsDefinition :: [InputValueDefinition]}
  deriving (Eq, Ord, Read, Show)

_InputFieldsDefinition = (Core.Name "hydra.ext.org.graphql.syntax.InputFieldsDefinition")

data InputObjectTypeExtension = 
  InputObjectTypeExtensionSequence InputObjectTypeExtension_Sequence |
  InputObjectTypeExtensionSequence2 InputObjectTypeExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeExtension = (Core.Name "hydra.ext.org.graphql.syntax.InputObjectTypeExtension")

_InputObjectTypeExtension_sequence = (Core.Name "sequence")

_InputObjectTypeExtension_sequence2 = (Core.Name "sequence2")

data InputObjectTypeExtension_Sequence = 
  InputObjectTypeExtension_Sequence {
    inputObjectTypeExtension_SequenceName :: Name,
    inputObjectTypeExtension_SequenceDirectives :: (Maybe Directives),
    inputObjectTypeExtension_SequenceInputFieldsDefinition :: InputFieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeExtension_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.InputObjectTypeExtension_Sequence")

_InputObjectTypeExtension_Sequence_Name = (Core.Name "Name")

_InputObjectTypeExtension_Sequence_Directives = (Core.Name "Directives")

_InputObjectTypeExtension_Sequence_InputFieldsDefinition = (Core.Name "InputFieldsDefinition")

data InputObjectTypeExtension_Sequence2 = 
  InputObjectTypeExtension_Sequence2 {
    inputObjectTypeExtension_Sequence2Name :: Name,
    inputObjectTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeExtension_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.InputObjectTypeExtension_Sequence2")

_InputObjectTypeExtension_Sequence2_Name = (Core.Name "Name")

_InputObjectTypeExtension_Sequence2_Directives = (Core.Name "Directives")

data DirectiveDefinition = 
  DirectiveDefinition {
    directiveDefinitionDescription :: (Maybe Description),
    directiveDefinitionName :: Name,
    directiveDefinitionArgumentsDefinition :: (Maybe ArgumentsDefinition),
    directiveDefinitionRepeatable :: (Maybe ()),
    directiveDefinitionDirectiveLocations :: DirectiveLocations}
  deriving (Eq, Ord, Read, Show)

_DirectiveDefinition = (Core.Name "hydra.ext.org.graphql.syntax.DirectiveDefinition")

_DirectiveDefinition_Description = (Core.Name "Description")

_DirectiveDefinition_Name = (Core.Name "Name")

_DirectiveDefinition_ArgumentsDefinition = (Core.Name "ArgumentsDefinition")

_DirectiveDefinition_Repeatable = (Core.Name "Repeatable")

_DirectiveDefinition_DirectiveLocations = (Core.Name "DirectiveLocations")

data DirectiveLocations = 
  DirectiveLocationsSequence DirectiveLocations_Sequence |
  DirectiveLocationsSequence2 DirectiveLocations_Sequence2
  deriving (Eq, Ord, Read, Show)

_DirectiveLocations = (Core.Name "hydra.ext.org.graphql.syntax.DirectiveLocations")

_DirectiveLocations_sequence = (Core.Name "sequence")

_DirectiveLocations_sequence2 = (Core.Name "sequence2")

data DirectiveLocations_Sequence = 
  DirectiveLocations_Sequence {
    directiveLocations_SequenceDirectiveLocations :: DirectiveLocations,
    directiveLocations_SequenceDirectiveLocation :: DirectiveLocation}
  deriving (Eq, Ord, Read, Show)

_DirectiveLocations_Sequence = (Core.Name "hydra.ext.org.graphql.syntax.DirectiveLocations_Sequence")

_DirectiveLocations_Sequence_DirectiveLocations = (Core.Name "DirectiveLocations")

_DirectiveLocations_Sequence_DirectiveLocation = (Core.Name "DirectiveLocation")

data DirectiveLocations_Sequence2 = 
  DirectiveLocations_Sequence2 {
    directiveLocations_Sequence2Or :: (Maybe ()),
    directiveLocations_Sequence2DirectiveLocation :: DirectiveLocation}
  deriving (Eq, Ord, Read, Show)

_DirectiveLocations_Sequence2 = (Core.Name "hydra.ext.org.graphql.syntax.DirectiveLocations_Sequence2")

_DirectiveLocations_Sequence2_Or = (Core.Name "Or")

_DirectiveLocations_Sequence2_DirectiveLocation = (Core.Name "DirectiveLocation")

data DirectiveLocation = 
  DirectiveLocationExecutable ExecutableDirectiveLocation |
  DirectiveLocationTypeSystem TypeSystemDirectiveLocation
  deriving (Eq, Ord, Read, Show)

_DirectiveLocation = (Core.Name "hydra.ext.org.graphql.syntax.DirectiveLocation")

_DirectiveLocation_executable = (Core.Name "executable")

_DirectiveLocation_typeSystem = (Core.Name "typeSystem")

data ExecutableDirectiveLocation = 
  ExecutableDirectiveLocationQUERY  |
  ExecutableDirectiveLocationMUTATION  |
  ExecutableDirectiveLocationSUBSCRIPTION  |
  ExecutableDirectiveLocationFIELD  |
  ExecutableDirectiveLocationFRAGMENTlowbarDEFINITION  |
  ExecutableDirectiveLocationFRAGMENTlowbarSPREAD  |
  ExecutableDirectiveLocationINLINElowbarFRAGMENT  |
  ExecutableDirectiveLocationVARIABLElowbarDEFINITION 
  deriving (Eq, Ord, Read, Show)

_ExecutableDirectiveLocation = (Core.Name "hydra.ext.org.graphql.syntax.ExecutableDirectiveLocation")

_ExecutableDirectiveLocation_QUERY = (Core.Name "QUERY")

_ExecutableDirectiveLocation_MUTATION = (Core.Name "MUTATION")

_ExecutableDirectiveLocation_SUBSCRIPTION = (Core.Name "SUBSCRIPTION")

_ExecutableDirectiveLocation_FIELD = (Core.Name "FIELD")

_ExecutableDirectiveLocation_FRAGMENTlowbarDEFINITION = (Core.Name "FRAGMENTlowbarDEFINITION")

_ExecutableDirectiveLocation_FRAGMENTlowbarSPREAD = (Core.Name "FRAGMENTlowbarSPREAD")

_ExecutableDirectiveLocation_INLINElowbarFRAGMENT = (Core.Name "INLINElowbarFRAGMENT")

_ExecutableDirectiveLocation_VARIABLElowbarDEFINITION = (Core.Name "VARIABLElowbarDEFINITION")

data TypeSystemDirectiveLocation = 
  TypeSystemDirectiveLocationSCHEMA  |
  TypeSystemDirectiveLocationSCALAR  |
  TypeSystemDirectiveLocationOBJECT  |
  TypeSystemDirectiveLocationFIELDlowbarDEFINITION  |
  TypeSystemDirectiveLocationARGUMENTlowbarDEFINITION  |
  TypeSystemDirectiveLocationINTERFACE  |
  TypeSystemDirectiveLocationUNION  |
  TypeSystemDirectiveLocationENUM  |
  TypeSystemDirectiveLocationENUMlowbarVALUE  |
  TypeSystemDirectiveLocationINPUTlowbarOBJECT  |
  TypeSystemDirectiveLocationINPUTlowbarFIELDlowbarDEFINITION 
  deriving (Eq, Ord, Read, Show)

_TypeSystemDirectiveLocation = (Core.Name "hydra.ext.org.graphql.syntax.TypeSystemDirectiveLocation")

_TypeSystemDirectiveLocation_SCHEMA = (Core.Name "SCHEMA")

_TypeSystemDirectiveLocation_SCALAR = (Core.Name "SCALAR")

_TypeSystemDirectiveLocation_OBJECT = (Core.Name "OBJECT")

_TypeSystemDirectiveLocation_FIELDlowbarDEFINITION = (Core.Name "FIELDlowbarDEFINITION")

_TypeSystemDirectiveLocation_ARGUMENTlowbarDEFINITION = (Core.Name "ARGUMENTlowbarDEFINITION")

_TypeSystemDirectiveLocation_INTERFACE = (Core.Name "INTERFACE")

_TypeSystemDirectiveLocation_UNION = (Core.Name "UNION")

_TypeSystemDirectiveLocation_ENUM = (Core.Name "ENUM")

_TypeSystemDirectiveLocation_ENUMlowbarVALUE = (Core.Name "ENUMlowbarVALUE")

_TypeSystemDirectiveLocation_INPUTlowbarOBJECT = (Core.Name "INPUTlowbarOBJECT")

_TypeSystemDirectiveLocation_INPUTlowbarFIELDlowbarDEFINITION = (Core.Name "INPUTlowbarFIELDlowbarDEFINITION")
