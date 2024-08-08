-- | A GraphQL model. Based on the (extended) BNF at:
-- |   https://spec.graphql.org/draft/#sec-Appendix-Grammar-Summary

module Hydra.Langs.Graphql.Syntax where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype Name = 
  Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra/langs/graphql/syntax.Name")

_Name_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype IntValue = 
  IntValue {
    unIntValue :: String}
  deriving (Eq, Ord, Read, Show)

_IntValue = (Core.Name "hydra/langs/graphql/syntax.IntValue")

_IntValue_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype FloatValue = 
  FloatValue {
    unFloatValue :: String}
  deriving (Eq, Ord, Read, Show)

_FloatValue = (Core.Name "hydra/langs/graphql/syntax.FloatValue")

_FloatValue_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype StringValue = 
  StringValue {
    unStringValue :: String}
  deriving (Eq, Ord, Read, Show)

_StringValue = (Core.Name "hydra/langs/graphql/syntax.StringValue")

_StringValue_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Document = 
  Document {
    unDocument :: [Definition]}
  deriving (Eq, Ord, Read, Show)

_Document = (Core.Name "hydra/langs/graphql/syntax.Document")

_Document_type_ = (Core.TypeList _Definition_type_)

data Definition = 
  DefinitionExecutable ExecutableDefinition |
  DefinitionTypeSystem TypeSystemDefinitionOrExtension
  deriving (Eq, Ord, Read, Show)

_Definition = (Core.Name "hydra/langs/graphql/syntax.Definition")

_Definition_executable = (Core.Name "executable")

_Definition_typeSystem = (Core.Name "typeSystem")

_Definition_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "executable"),
      Core.fieldTypeType = _ExecutableDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeSystem"),
      Core.fieldTypeType = _TypeSystemDefinitionOrExtension_type_}]}))

newtype ExecutableDocument = 
  ExecutableDocument {
    unExecutableDocument :: [ExecutableDefinition]}
  deriving (Eq, Ord, Read, Show)

_ExecutableDocument = (Core.Name "hydra/langs/graphql/syntax.ExecutableDocument")

_ExecutableDocument_type_ = (Core.TypeList _ExecutableDefinition_type_)

data ExecutableDefinition = 
  ExecutableDefinitionOperation OperationDefinition |
  ExecutableDefinitionFragment FragmentDefinition
  deriving (Eq, Ord, Read, Show)

_ExecutableDefinition = (Core.Name "hydra/langs/graphql/syntax.ExecutableDefinition")

_ExecutableDefinition_operation = (Core.Name "operation")

_ExecutableDefinition_fragment = (Core.Name "fragment")

_ExecutableDefinition_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operation"),
      Core.fieldTypeType = _OperationDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fragment"),
      Core.fieldTypeType = _FragmentDefinition_type_}]}))

data OperationDefinition = 
  OperationDefinitionSequence OperationDefinition_Sequence |
  OperationDefinitionSelectionSet SelectionSet
  deriving (Eq, Ord, Read, Show)

_OperationDefinition = (Core.Name "hydra/langs/graphql/syntax.OperationDefinition")

_OperationDefinition_sequence = (Core.Name "sequence")

_OperationDefinition_selectionSet = (Core.Name "selectionSet")

_OperationDefinition_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _OperationDefinition_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "selectionSet"),
      Core.fieldTypeType = _SelectionSet_type_}]}))

data OperationDefinition_Sequence = 
  OperationDefinition_Sequence {
    operationDefinition_SequenceOperationType :: OperationType,
    operationDefinition_SequenceName :: (Maybe Name),
    operationDefinition_SequenceVariablesDefinition :: (Maybe VariablesDefinition),
    operationDefinition_SequenceDirectives :: (Maybe Directives),
    operationDefinition_SequenceSelectionSet :: SelectionSet}
  deriving (Eq, Ord, Read, Show)

_OperationDefinition_Sequence = (Core.Name "hydra/langs/graphql/syntax.OperationDefinition.Sequence")

_OperationDefinition_Sequence_operationType = (Core.Name "operationType")

_OperationDefinition_Sequence_name = (Core.Name "name")

_OperationDefinition_Sequence_variablesDefinition = (Core.Name "variablesDefinition")

_OperationDefinition_Sequence_directives = (Core.Name "directives")

_OperationDefinition_Sequence_selectionSet = (Core.Name "selectionSet")

_OperationDefinition_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operationType"),
      Core.fieldTypeType = _OperationType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = (Core.TypeOptional _Name_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variablesDefinition"),
      Core.fieldTypeType = (Core.TypeOptional _VariablesDefinition_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "selectionSet"),
      Core.fieldTypeType = _SelectionSet_type_}]}))

data OperationType = 
  OperationTypeQuery  |
  OperationTypeMutation  |
  OperationTypeSubscription 
  deriving (Eq, Ord, Read, Show)

_OperationType = (Core.Name "hydra/langs/graphql/syntax.OperationType")

_OperationType_query = (Core.Name "query")

_OperationType_mutation = (Core.Name "mutation")

_OperationType_subscription = (Core.Name "subscription")

_OperationType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "query"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mutation"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subscription"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype SelectionSet = 
  SelectionSet {
    unSelectionSet :: [Selection]}
  deriving (Eq, Ord, Read, Show)

_SelectionSet = (Core.Name "hydra/langs/graphql/syntax.SelectionSet")

_SelectionSet_type_ = (Core.TypeList _Selection_type_)

data Selection = 
  SelectionField Field |
  SelectionFragmentSpread FragmentSpread |
  SelectionInlineFragment InlineFragment
  deriving (Eq, Ord, Read, Show)

_Selection = (Core.Name "hydra/langs/graphql/syntax.Selection")

_Selection_field = (Core.Name "field")

_Selection_fragmentSpread = (Core.Name "fragmentSpread")

_Selection_inlineFragment = (Core.Name "inlineFragment")

_Selection_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "field"),
      Core.fieldTypeType = _Field_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fragmentSpread"),
      Core.fieldTypeType = _FragmentSpread_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inlineFragment"),
      Core.fieldTypeType = _InlineFragment_type_}]}))

data Field = 
  Field {
    fieldAlias :: (Maybe Alias),
    fieldName :: Name,
    fieldArguments :: (Maybe Arguments),
    fieldDirectives :: (Maybe Directives),
    fieldSelectionSet :: (Maybe SelectionSet)}
  deriving (Eq, Ord, Read, Show)

_Field = (Core.Name "hydra/langs/graphql/syntax.Field")

_Field_alias = (Core.Name "alias")

_Field_name = (Core.Name "name")

_Field_arguments = (Core.Name "arguments")

_Field_directives = (Core.Name "directives")

_Field_selectionSet = (Core.Name "selectionSet")

_Field_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "alias"),
      Core.fieldTypeType = (Core.TypeOptional _Alias_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeOptional _Arguments_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "selectionSet"),
      Core.fieldTypeType = (Core.TypeOptional _SelectionSet_type_)}]}))

data Alias = 
  AliasName Name |
  AliasColon 
  deriving (Eq, Ord, Read, Show)

_Alias = (Core.Name "hydra/langs/graphql/syntax.Alias")

_Alias_name = (Core.Name "name")

_Alias_colon = (Core.Name "colon")

_Alias_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "colon"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype Arguments = 
  Arguments {
    unArguments :: [Argument]}
  deriving (Eq, Ord, Read, Show)

_Arguments = (Core.Name "hydra/langs/graphql/syntax.Arguments")

_Arguments_type_ = (Core.TypeList _Argument_type_)

data Argument = 
  Argument {
    argumentName :: Name,
    argumentValue :: Value}
  deriving (Eq, Ord, Read, Show)

_Argument = (Core.Name "hydra/langs/graphql/syntax.Argument")

_Argument_name = (Core.Name "name")

_Argument_value = (Core.Name "value")

_Argument_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Value_type_}]}))

data FragmentSpread = 
  FragmentSpread {
    fragmentSpreadFragmentName :: FragmentName,
    fragmentSpreadDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_FragmentSpread = (Core.Name "hydra/langs/graphql/syntax.FragmentSpread")

_FragmentSpread_fragmentName = (Core.Name "fragmentName")

_FragmentSpread_directives = (Core.Name "directives")

_FragmentSpread_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fragmentName"),
      Core.fieldTypeType = _FragmentName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)}]}))

data InlineFragment = 
  InlineFragment {
    inlineFragmentTypeCondition :: (Maybe TypeCondition),
    inlineFragmentDirectives :: (Maybe Directives),
    inlineFragmentSelectionSet :: SelectionSet}
  deriving (Eq, Ord, Read, Show)

_InlineFragment = (Core.Name "hydra/langs/graphql/syntax.InlineFragment")

_InlineFragment_typeCondition = (Core.Name "typeCondition")

_InlineFragment_directives = (Core.Name "directives")

_InlineFragment_selectionSet = (Core.Name "selectionSet")

_InlineFragment_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeCondition"),
      Core.fieldTypeType = (Core.TypeOptional _TypeCondition_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "selectionSet"),
      Core.fieldTypeType = _SelectionSet_type_}]}))

data FragmentDefinition = 
  FragmentDefinition {
    fragmentDefinitionFragmentName :: FragmentName,
    fragmentDefinitionTypeCondition :: TypeCondition,
    fragmentDefinitionDirectives :: (Maybe Directives),
    fragmentDefinitionSelectionSet :: SelectionSet}
  deriving (Eq, Ord, Read, Show)

_FragmentDefinition = (Core.Name "hydra/langs/graphql/syntax.FragmentDefinition")

_FragmentDefinition_fragmentName = (Core.Name "fragmentName")

_FragmentDefinition_typeCondition = (Core.Name "typeCondition")

_FragmentDefinition_directives = (Core.Name "directives")

_FragmentDefinition_selectionSet = (Core.Name "selectionSet")

_FragmentDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fragmentName"),
      Core.fieldTypeType = _FragmentName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeCondition"),
      Core.fieldTypeType = _TypeCondition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "selectionSet"),
      Core.fieldTypeType = _SelectionSet_type_}]}))

newtype FragmentName = 
  FragmentName {
    unFragmentName :: Name}
  deriving (Eq, Ord, Read, Show)

_FragmentName = (Core.Name "hydra/langs/graphql/syntax.FragmentName")

_FragmentName_type_ = _Name_type_

data TypeCondition = 
  TypeConditionOn  |
  TypeConditionNamedType NamedType
  deriving (Eq, Ord, Read, Show)

_TypeCondition = (Core.Name "hydra/langs/graphql/syntax.TypeCondition")

_TypeCondition_on = (Core.Name "on")

_TypeCondition_namedType = (Core.Name "namedType")

_TypeCondition_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "on"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namedType"),
      Core.fieldTypeType = _NamedType_type_}]}))

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

_Value = (Core.Name "hydra/langs/graphql/syntax.Value")

_Value_variable = (Core.Name "variable")

_Value_int = (Core.Name "int")

_Value_float = (Core.Name "float")

_Value_string = (Core.Name "string")

_Value_boolean = (Core.Name "boolean")

_Value_null = (Core.Name "null")

_Value_enum = (Core.Name "enum")

_Value_list = (Core.Name "list")

_Value_object = (Core.Name "object")

_Value_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Variable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = _IntValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = _FloatValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = _BooleanValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "null"),
      Core.fieldTypeType = _NullValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enum"),
      Core.fieldTypeType = _EnumValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = _ListValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _ObjectValue_type_}]}))

data BooleanValue = 
  BooleanValueTrue  |
  BooleanValueFalse 
  deriving (Eq, Ord, Read, Show)

_BooleanValue = (Core.Name "hydra/langs/graphql/syntax.BooleanValue")

_BooleanValue_true = (Core.Name "true")

_BooleanValue_false = (Core.Name "false")

_BooleanValue_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "true"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "false"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data NullValue = 
  NullValue {}
  deriving (Eq, Ord, Read, Show)

_NullValue = (Core.Name "hydra/langs/graphql/syntax.NullValue")

_NullValue_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype EnumValue = 
  EnumValue {
    unEnumValue :: Name}
  deriving (Eq, Ord, Read, Show)

_EnumValue = (Core.Name "hydra/langs/graphql/syntax.EnumValue")

_EnumValue_type_ = _Name_type_

data ListValue = 
  ListValueSequence ListValue_Sequence |
  ListValueSequence2 [Value]
  deriving (Eq, Ord, Read, Show)

_ListValue = (Core.Name "hydra/langs/graphql/syntax.ListValue")

_ListValue_sequence = (Core.Name "sequence")

_ListValue_sequence2 = (Core.Name "sequence2")

_ListValue_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _ListValue_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = (Core.TypeList _Value_type_)}]}))

data ListValue_Sequence = 
  ListValue_Sequence {}
  deriving (Eq, Ord, Read, Show)

_ListValue_Sequence = (Core.Name "hydra/langs/graphql/syntax.ListValue.Sequence")

_ListValue_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ObjectValue = 
  ObjectValueSequence ObjectValue_Sequence |
  ObjectValueSequence2 [ObjectField]
  deriving (Eq, Ord, Read, Show)

_ObjectValue = (Core.Name "hydra/langs/graphql/syntax.ObjectValue")

_ObjectValue_sequence = (Core.Name "sequence")

_ObjectValue_sequence2 = (Core.Name "sequence2")

_ObjectValue_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _ObjectValue_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = (Core.TypeList _ObjectField_type_)}]}))

data ObjectValue_Sequence = 
  ObjectValue_Sequence {}
  deriving (Eq, Ord, Read, Show)

_ObjectValue_Sequence = (Core.Name "hydra/langs/graphql/syntax.ObjectValue.Sequence")

_ObjectValue_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ObjectField = 
  ObjectField {
    objectFieldName :: Name,
    objectFieldValue :: Value}
  deriving (Eq, Ord, Read, Show)

_ObjectField = (Core.Name "hydra/langs/graphql/syntax.ObjectField")

_ObjectField_name = (Core.Name "name")

_ObjectField_value = (Core.Name "value")

_ObjectField_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Value_type_}]}))

data VariablesDefinition = 
  VariablesDefinition {
    variablesDefinitionVariable :: Variable,
    variablesDefinitionType :: Type,
    variablesDefinitionDefaultValue :: (Maybe DefaultValue),
    variablesDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_VariablesDefinition = (Core.Name "hydra/langs/graphql/syntax.VariablesDefinition")

_VariablesDefinition_variable = (Core.Name "variable")

_VariablesDefinition_type = (Core.Name "type")

_VariablesDefinition_defaultValue = (Core.Name "defaultValue")

_VariablesDefinition_directives = (Core.Name "directives")

_VariablesDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Variable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "defaultValue"),
      Core.fieldTypeType = (Core.TypeOptional _DefaultValue_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)}]}))

newtype Variable = 
  Variable {
    unVariable :: Name}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra/langs/graphql/syntax.Variable")

_Variable_type_ = _Name_type_

newtype DefaultValue = 
  DefaultValue {
    unDefaultValue :: Value}
  deriving (Eq, Ord, Read, Show)

_DefaultValue = (Core.Name "hydra/langs/graphql/syntax.DefaultValue")

_DefaultValue_type_ = _Value_type_

data Type = 
  TypeNamed NamedType |
  TypeList ListType |
  TypeNonNull NonNullType
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra/langs/graphql/syntax.Type")

_Type_named = (Core.Name "named")

_Type_list = (Core.Name "list")

_Type_nonNull = (Core.Name "nonNull")

_Type_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "named"),
      Core.fieldTypeType = _NamedType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = _ListType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nonNull"),
      Core.fieldTypeType = _NonNullType_type_}]}))

newtype NamedType = 
  NamedType {
    unNamedType :: Name}
  deriving (Eq, Ord, Read, Show)

_NamedType = (Core.Name "hydra/langs/graphql/syntax.NamedType")

_NamedType_type_ = _Name_type_

newtype ListType = 
  ListType {
    unListType :: Type}
  deriving (Eq, Ord, Read, Show)

_ListType = (Core.Name "hydra/langs/graphql/syntax.ListType")

_ListType_type_ = _Type_type_

data NonNullType = 
  NonNullTypeNamed NamedType |
  NonNullTypeList ListType
  deriving (Eq, Ord, Read, Show)

_NonNullType = (Core.Name "hydra/langs/graphql/syntax.NonNullType")

_NonNullType_named = (Core.Name "named")

_NonNullType_list = (Core.Name "list")

_NonNullType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "named"),
      Core.fieldTypeType = _NamedType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = _ListType_type_}]}))

newtype Directives = 
  Directives {
    unDirectives :: [Directive]}
  deriving (Eq, Ord, Read, Show)

_Directives = (Core.Name "hydra/langs/graphql/syntax.Directives")

_Directives_type_ = (Core.TypeList _Directive_type_)

data Directive = 
  Directive {
    directiveName :: Name,
    directiveArguments :: (Maybe Arguments)}
  deriving (Eq, Ord, Read, Show)

_Directive = (Core.Name "hydra/langs/graphql/syntax.Directive")

_Directive_name = (Core.Name "name")

_Directive_arguments = (Core.Name "arguments")

_Directive_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeOptional _Arguments_type_)}]}))

newtype TypeSystemDocment = 
  TypeSystemDocment {
    unTypeSystemDocment :: [TypeSystemDefinition]}
  deriving (Eq, Ord, Read, Show)

_TypeSystemDocment = (Core.Name "hydra/langs/graphql/syntax.TypeSystemDocment")

_TypeSystemDocment_type_ = (Core.TypeList _TypeSystemDefinition_type_)

data TypeSystemDefinition = 
  TypeSystemDefinitionSchema SchemaDefinition |
  TypeSystemDefinitionType TypeDefinition |
  TypeSystemDefinitionDirective DirectiveDefinition
  deriving (Eq, Ord, Read, Show)

_TypeSystemDefinition = (Core.Name "hydra/langs/graphql/syntax.TypeSystemDefinition")

_TypeSystemDefinition_schema = (Core.Name "schema")

_TypeSystemDefinition_type = (Core.Name "type")

_TypeSystemDefinition_directive = (Core.Name "directive")

_TypeSystemDefinition_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "schema"),
      Core.fieldTypeType = _SchemaDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _TypeDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directive"),
      Core.fieldTypeType = _DirectiveDefinition_type_}]}))

newtype TypeSystemExtensionDocument = 
  TypeSystemExtensionDocument {
    unTypeSystemExtensionDocument :: [TypeSystemDefinitionOrExtension]}
  deriving (Eq, Ord, Read, Show)

_TypeSystemExtensionDocument = (Core.Name "hydra/langs/graphql/syntax.TypeSystemExtensionDocument")

_TypeSystemExtensionDocument_type_ = (Core.TypeList _TypeSystemDefinitionOrExtension_type_)

data TypeSystemDefinitionOrExtension = 
  TypeSystemDefinitionOrExtensionDefinition TypeSystemDefinition |
  TypeSystemDefinitionOrExtensionExtension TypeSystemExtension
  deriving (Eq, Ord, Read, Show)

_TypeSystemDefinitionOrExtension = (Core.Name "hydra/langs/graphql/syntax.TypeSystemDefinitionOrExtension")

_TypeSystemDefinitionOrExtension_definition = (Core.Name "definition")

_TypeSystemDefinitionOrExtension_extension = (Core.Name "extension")

_TypeSystemDefinitionOrExtension_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "definition"),
      Core.fieldTypeType = _TypeSystemDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "extension"),
      Core.fieldTypeType = _TypeSystemExtension_type_}]}))

data TypeSystemExtension = 
  TypeSystemExtensionSchema SchemaExtension |
  TypeSystemExtensionType TypeExtension
  deriving (Eq, Ord, Read, Show)

_TypeSystemExtension = (Core.Name "hydra/langs/graphql/syntax.TypeSystemExtension")

_TypeSystemExtension_schema = (Core.Name "schema")

_TypeSystemExtension_type = (Core.Name "type")

_TypeSystemExtension_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "schema"),
      Core.fieldTypeType = _SchemaExtension_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _TypeExtension_type_}]}))

data SchemaDefinition = 
  SchemaDefinition {
    schemaDefinitionDescription :: (Maybe Description),
    schemaDefinitionDirectives :: (Maybe Directives),
    schemaDefinitionRootOperationTypeDefinition :: RootOperationTypeDefinition}
  deriving (Eq, Ord, Read, Show)

_SchemaDefinition = (Core.Name "hydra/langs/graphql/syntax.SchemaDefinition")

_SchemaDefinition_description = (Core.Name "description")

_SchemaDefinition_directives = (Core.Name "directives")

_SchemaDefinition_rootOperationTypeDefinition = (Core.Name "rootOperationTypeDefinition")

_SchemaDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rootOperationTypeDefinition"),
      Core.fieldTypeType = _RootOperationTypeDefinition_type_}]}))

data SchemaExtension = 
  SchemaExtensionSequence SchemaExtension_Sequence |
  SchemaExtensionSequence2 Directives
  deriving (Eq, Ord, Read, Show)

_SchemaExtension = (Core.Name "hydra/langs/graphql/syntax.SchemaExtension")

_SchemaExtension_sequence = (Core.Name "sequence")

_SchemaExtension_sequence2 = (Core.Name "sequence2")

_SchemaExtension_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _SchemaExtension_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _Directives_type_}]}))

data SchemaExtension_Sequence = 
  SchemaExtension_Sequence {
    schemaExtension_SequenceDirectives :: (Maybe Directives),
    schemaExtension_SequenceRootOperationTypeDefinition :: RootOperationTypeDefinition}
  deriving (Eq, Ord, Read, Show)

_SchemaExtension_Sequence = (Core.Name "hydra/langs/graphql/syntax.SchemaExtension.Sequence")

_SchemaExtension_Sequence_directives = (Core.Name "directives")

_SchemaExtension_Sequence_rootOperationTypeDefinition = (Core.Name "rootOperationTypeDefinition")

_SchemaExtension_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rootOperationTypeDefinition"),
      Core.fieldTypeType = _RootOperationTypeDefinition_type_}]}))

data RootOperationTypeDefinition = 
  RootOperationTypeDefinition {
    rootOperationTypeDefinitionOperationType :: OperationType,
    rootOperationTypeDefinitionNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_RootOperationTypeDefinition = (Core.Name "hydra/langs/graphql/syntax.RootOperationTypeDefinition")

_RootOperationTypeDefinition_operationType = (Core.Name "operationType")

_RootOperationTypeDefinition_namedType = (Core.Name "namedType")

_RootOperationTypeDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operationType"),
      Core.fieldTypeType = _OperationType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namedType"),
      Core.fieldTypeType = _NamedType_type_}]}))

newtype Description = 
  Description {
    unDescription :: StringValue}
  deriving (Eq, Ord, Read, Show)

_Description = (Core.Name "hydra/langs/graphql/syntax.Description")

_Description_type_ = _StringValue_type_

data TypeDefinition = 
  TypeDefinitionScalar ScalarTypeDefinition |
  TypeDefinitionObject ObjectTypeDefinition |
  TypeDefinitionInterface InterfaceTypeDefinition |
  TypeDefinitionUnion UnionTypeDefinition |
  TypeDefinitionEnum EnumTypeDefinition |
  TypeDefinitionInputObject InputObjectTypeDefinition
  deriving (Eq, Ord, Read, Show)

_TypeDefinition = (Core.Name "hydra/langs/graphql/syntax.TypeDefinition")

_TypeDefinition_scalar = (Core.Name "scalar")

_TypeDefinition_object = (Core.Name "object")

_TypeDefinition_interface = (Core.Name "interface")

_TypeDefinition_union = (Core.Name "union")

_TypeDefinition_enum = (Core.Name "enum")

_TypeDefinition_inputObject = (Core.Name "inputObject")

_TypeDefinition_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scalar"),
      Core.fieldTypeType = _ScalarTypeDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _ObjectTypeDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interface"),
      Core.fieldTypeType = _InterfaceTypeDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "union"),
      Core.fieldTypeType = _UnionTypeDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enum"),
      Core.fieldTypeType = _EnumTypeDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inputObject"),
      Core.fieldTypeType = _InputObjectTypeDefinition_type_}]}))

data TypeExtension = 
  TypeExtensionScalar ScalarTypeExtension |
  TypeExtensionObject ObjectTypeExtension |
  TypeExtensionInterface InterfaceTypeExtension |
  TypeExtensionUnion UnionTypeExtension |
  TypeExtensionEnum EnumTypeExtension |
  TypeExtensionInputObject InputObjectTypeExtension
  deriving (Eq, Ord, Read, Show)

_TypeExtension = (Core.Name "hydra/langs/graphql/syntax.TypeExtension")

_TypeExtension_scalar = (Core.Name "scalar")

_TypeExtension_object = (Core.Name "object")

_TypeExtension_interface = (Core.Name "interface")

_TypeExtension_union = (Core.Name "union")

_TypeExtension_enum = (Core.Name "enum")

_TypeExtension_inputObject = (Core.Name "inputObject")

_TypeExtension_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scalar"),
      Core.fieldTypeType = _ScalarTypeExtension_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _ObjectTypeExtension_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interface"),
      Core.fieldTypeType = _InterfaceTypeExtension_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "union"),
      Core.fieldTypeType = _UnionTypeExtension_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enum"),
      Core.fieldTypeType = _EnumTypeExtension_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inputObject"),
      Core.fieldTypeType = _InputObjectTypeExtension_type_}]}))

data ScalarTypeDefinition = 
  ScalarTypeDefinition {
    scalarTypeDefinitionDescription :: (Maybe Description),
    scalarTypeDefinitionName :: Name,
    scalarTypeDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_ScalarTypeDefinition = (Core.Name "hydra/langs/graphql/syntax.ScalarTypeDefinition")

_ScalarTypeDefinition_description = (Core.Name "description")

_ScalarTypeDefinition_name = (Core.Name "name")

_ScalarTypeDefinition_directives = (Core.Name "directives")

_ScalarTypeDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)}]}))

data ScalarTypeExtension = 
  ScalarTypeExtension {
    scalarTypeExtensionName :: Name,
    scalarTypeExtensionDirectives :: Directives}
  deriving (Eq, Ord, Read, Show)

_ScalarTypeExtension = (Core.Name "hydra/langs/graphql/syntax.ScalarTypeExtension")

_ScalarTypeExtension_name = (Core.Name "name")

_ScalarTypeExtension_directives = (Core.Name "directives")

_ScalarTypeExtension_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = _Directives_type_}]}))

data ObjectTypeDefinition = 
  ObjectTypeDefinition {
    objectTypeDefinitionDescription :: (Maybe Description),
    objectTypeDefinitionName :: Name,
    objectTypeDefinitionImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeDefinitionDirectives :: (Maybe Directives),
    objectTypeDefinitionFieldsDefinition :: (Maybe FieldsDefinition)}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeDefinition = (Core.Name "hydra/langs/graphql/syntax.ObjectTypeDefinition")

_ObjectTypeDefinition_description = (Core.Name "description")

_ObjectTypeDefinition_name = (Core.Name "name")

_ObjectTypeDefinition_implementsInterfaces = (Core.Name "implementsInterfaces")

_ObjectTypeDefinition_directives = (Core.Name "directives")

_ObjectTypeDefinition_fieldsDefinition = (Core.Name "fieldsDefinition")

_ObjectTypeDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = (Core.TypeOptional _ImplementsInterfaces_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fieldsDefinition"),
      Core.fieldTypeType = (Core.TypeOptional _FieldsDefinition_type_)}]}))

data ObjectTypeExtension = 
  ObjectTypeExtensionSequence ObjectTypeExtension_Sequence |
  ObjectTypeExtensionSequence2 ObjectTypeExtension_Sequence2 |
  ObjectTypeExtensionSequence3 ObjectTypeExtension_Sequence3
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension = (Core.Name "hydra/langs/graphql/syntax.ObjectTypeExtension")

_ObjectTypeExtension_sequence = (Core.Name "sequence")

_ObjectTypeExtension_sequence2 = (Core.Name "sequence2")

_ObjectTypeExtension_sequence3 = (Core.Name "sequence3")

_ObjectTypeExtension_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _ObjectTypeExtension_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _ObjectTypeExtension_Sequence2_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence3"),
      Core.fieldTypeType = _ObjectTypeExtension_Sequence3_type_}]}))

data ObjectTypeExtension_Sequence = 
  ObjectTypeExtension_Sequence {
    objectTypeExtension_SequenceName :: Name,
    objectTypeExtension_SequenceImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeExtension_SequenceDirectives :: (Maybe Directives),
    objectTypeExtension_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension_Sequence = (Core.Name "hydra/langs/graphql/syntax.ObjectTypeExtension.Sequence")

_ObjectTypeExtension_Sequence_name = (Core.Name "name")

_ObjectTypeExtension_Sequence_implementsInterfaces = (Core.Name "implementsInterfaces")

_ObjectTypeExtension_Sequence_directives = (Core.Name "directives")

_ObjectTypeExtension_Sequence_fieldsDefinition = (Core.Name "fieldsDefinition")

_ObjectTypeExtension_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = (Core.TypeOptional _ImplementsInterfaces_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fieldsDefinition"),
      Core.fieldTypeType = _FieldsDefinition_type_}]}))

data ObjectTypeExtension_Sequence2 = 
  ObjectTypeExtension_Sequence2 {
    objectTypeExtension_Sequence2Name :: Name,
    objectTypeExtension_Sequence2ImplementsInterfaces :: (Maybe ImplementsInterfaces),
    objectTypeExtension_Sequence2Directives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.ObjectTypeExtension.Sequence2")

_ObjectTypeExtension_Sequence2_name = (Core.Name "name")

_ObjectTypeExtension_Sequence2_implementsInterfaces = (Core.Name "implementsInterfaces")

_ObjectTypeExtension_Sequence2_directives = (Core.Name "directives")

_ObjectTypeExtension_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = (Core.TypeOptional _ImplementsInterfaces_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)}]}))

data ObjectTypeExtension_Sequence3 = 
  ObjectTypeExtension_Sequence3 {
    objectTypeExtension_Sequence3Name :: Name,
    objectTypeExtension_Sequence3ImplementsInterfaces :: ImplementsInterfaces}
  deriving (Eq, Ord, Read, Show)

_ObjectTypeExtension_Sequence3 = (Core.Name "hydra/langs/graphql/syntax.ObjectTypeExtension.Sequence3")

_ObjectTypeExtension_Sequence3_name = (Core.Name "name")

_ObjectTypeExtension_Sequence3_implementsInterfaces = (Core.Name "implementsInterfaces")

_ObjectTypeExtension_Sequence3_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = _ImplementsInterfaces_type_}]}))

data ImplementsInterfaces = 
  ImplementsInterfacesSequence ImplementsInterfaces_Sequence |
  ImplementsInterfacesSequence2 ImplementsInterfaces_Sequence2
  deriving (Eq, Ord, Read, Show)

_ImplementsInterfaces = (Core.Name "hydra/langs/graphql/syntax.ImplementsInterfaces")

_ImplementsInterfaces_sequence = (Core.Name "sequence")

_ImplementsInterfaces_sequence2 = (Core.Name "sequence2")

_ImplementsInterfaces_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _ImplementsInterfaces_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _ImplementsInterfaces_Sequence2_type_}]}))

data ImplementsInterfaces_Sequence = 
  ImplementsInterfaces_Sequence {
    implementsInterfaces_SequenceImplementsInterfaces :: ImplementsInterfaces,
    implementsInterfaces_SequenceNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_ImplementsInterfaces_Sequence = (Core.Name "hydra/langs/graphql/syntax.ImplementsInterfaces.Sequence")

_ImplementsInterfaces_Sequence_implementsInterfaces = (Core.Name "implementsInterfaces")

_ImplementsInterfaces_Sequence_namedType = (Core.Name "namedType")

_ImplementsInterfaces_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = _ImplementsInterfaces_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namedType"),
      Core.fieldTypeType = _NamedType_type_}]}))

data ImplementsInterfaces_Sequence2 = 
  ImplementsInterfaces_Sequence2 {
    implementsInterfaces_Sequence2Amp :: (Maybe ()),
    implementsInterfaces_Sequence2NamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_ImplementsInterfaces_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.ImplementsInterfaces.Sequence2")

_ImplementsInterfaces_Sequence2_amp = (Core.Name "amp")

_ImplementsInterfaces_Sequence2_namedType = (Core.Name "namedType")

_ImplementsInterfaces_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "amp"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namedType"),
      Core.fieldTypeType = _NamedType_type_}]}))

newtype FieldsDefinition = 
  FieldsDefinition {
    unFieldsDefinition :: [FieldDefinition]}
  deriving (Eq, Ord, Read, Show)

_FieldsDefinition = (Core.Name "hydra/langs/graphql/syntax.FieldsDefinition")

_FieldsDefinition_type_ = (Core.TypeList _FieldDefinition_type_)

data FieldDefinition = 
  FieldDefinition {
    fieldDefinitionDescription :: (Maybe Description),
    fieldDefinitionName :: Name,
    fieldDefinitionArgumentsDefinition :: (Maybe ArgumentsDefinition),
    fieldDefinitionType :: Type,
    fieldDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_FieldDefinition = (Core.Name "hydra/langs/graphql/syntax.FieldDefinition")

_FieldDefinition_description = (Core.Name "description")

_FieldDefinition_name = (Core.Name "name")

_FieldDefinition_argumentsDefinition = (Core.Name "argumentsDefinition")

_FieldDefinition_type = (Core.Name "type")

_FieldDefinition_directives = (Core.Name "directives")

_FieldDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "argumentsDefinition"),
      Core.fieldTypeType = (Core.TypeOptional _ArgumentsDefinition_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)}]}))

newtype ArgumentsDefinition = 
  ArgumentsDefinition {
    unArgumentsDefinition :: [InputValueDefinition]}
  deriving (Eq, Ord, Read, Show)

_ArgumentsDefinition = (Core.Name "hydra/langs/graphql/syntax.ArgumentsDefinition")

_ArgumentsDefinition_type_ = (Core.TypeList _InputValueDefinition_type_)

data InputValueDefinition = 
  InputValueDefinition {
    inputValueDefinitionDescription :: (Maybe Description),
    inputValueDefinitionName :: Name,
    inputValueDefinitionType :: Type,
    inputValueDefinitionDefaultValue :: (Maybe DefaultValue),
    inputValueDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_InputValueDefinition = (Core.Name "hydra/langs/graphql/syntax.InputValueDefinition")

_InputValueDefinition_description = (Core.Name "description")

_InputValueDefinition_name = (Core.Name "name")

_InputValueDefinition_type = (Core.Name "type")

_InputValueDefinition_defaultValue = (Core.Name "defaultValue")

_InputValueDefinition_directives = (Core.Name "directives")

_InputValueDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _Type_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "defaultValue"),
      Core.fieldTypeType = (Core.TypeOptional _DefaultValue_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)}]}))

data InterfaceTypeDefinition = 
  InterfaceTypeDefinitionSequence InterfaceTypeDefinition_Sequence |
  InterfaceTypeDefinitionSequence2 InterfaceTypeDefinition_Sequence2
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeDefinition = (Core.Name "hydra/langs/graphql/syntax.InterfaceTypeDefinition")

_InterfaceTypeDefinition_sequence = (Core.Name "sequence")

_InterfaceTypeDefinition_sequence2 = (Core.Name "sequence2")

_InterfaceTypeDefinition_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _InterfaceTypeDefinition_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _InterfaceTypeDefinition_Sequence2_type_}]}))

data InterfaceTypeDefinition_Sequence = 
  InterfaceTypeDefinition_Sequence {
    interfaceTypeDefinition_SequenceDescription :: (Maybe Description),
    interfaceTypeDefinition_SequenceName :: Name,
    interfaceTypeDefinition_SequenceImplementsInterfaces :: (Maybe ImplementsInterfaces),
    interfaceTypeDefinition_SequenceDirectives :: (Maybe Directives),
    interfaceTypeDefinition_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeDefinition_Sequence = (Core.Name "hydra/langs/graphql/syntax.InterfaceTypeDefinition.Sequence")

_InterfaceTypeDefinition_Sequence_description = (Core.Name "description")

_InterfaceTypeDefinition_Sequence_name = (Core.Name "name")

_InterfaceTypeDefinition_Sequence_implementsInterfaces = (Core.Name "implementsInterfaces")

_InterfaceTypeDefinition_Sequence_directives = (Core.Name "directives")

_InterfaceTypeDefinition_Sequence_fieldsDefinition = (Core.Name "fieldsDefinition")

_InterfaceTypeDefinition_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = (Core.TypeOptional _ImplementsInterfaces_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fieldsDefinition"),
      Core.fieldTypeType = _FieldsDefinition_type_}]}))

data InterfaceTypeDefinition_Sequence2 = 
  InterfaceTypeDefinition_Sequence2 {
    interfaceTypeDefinition_Sequence2Description :: (Maybe Description),
    interfaceTypeDefinition_Sequence2Name :: Name,
    interfaceTypeDefinition_Sequence2ImplementsInterfaces :: ImplementsInterfaces,
    interfaceTypeDefinition_Sequence2Directives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeDefinition_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.InterfaceTypeDefinition.Sequence2")

_InterfaceTypeDefinition_Sequence2_description = (Core.Name "description")

_InterfaceTypeDefinition_Sequence2_name = (Core.Name "name")

_InterfaceTypeDefinition_Sequence2_implementsInterfaces = (Core.Name "implementsInterfaces")

_InterfaceTypeDefinition_Sequence2_directives = (Core.Name "directives")

_InterfaceTypeDefinition_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = _ImplementsInterfaces_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)}]}))

data InterfaceTypeExtension = 
  InterfaceTypeExtensionSequence InterfaceTypeExtension_Sequence |
  InterfaceTypeExtensionSequence2 InterfaceTypeExtension_Sequence2 |
  InterfaceTypeExtensionSequence3 InterfaceTypeExtension_Sequence3
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension = (Core.Name "hydra/langs/graphql/syntax.InterfaceTypeExtension")

_InterfaceTypeExtension_sequence = (Core.Name "sequence")

_InterfaceTypeExtension_sequence2 = (Core.Name "sequence2")

_InterfaceTypeExtension_sequence3 = (Core.Name "sequence3")

_InterfaceTypeExtension_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _InterfaceTypeExtension_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _InterfaceTypeExtension_Sequence2_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence3"),
      Core.fieldTypeType = _InterfaceTypeExtension_Sequence3_type_}]}))

data InterfaceTypeExtension_Sequence = 
  InterfaceTypeExtension_Sequence {
    interfaceTypeExtension_SequenceName :: Name,
    interfaceTypeExtension_SequenceImplementsInterfaces :: (Maybe ImplementsInterfaces),
    interfaceTypeExtension_SequenceDirectives :: (Maybe Directives),
    interfaceTypeExtension_SequenceFieldsDefinition :: FieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension_Sequence = (Core.Name "hydra/langs/graphql/syntax.InterfaceTypeExtension.Sequence")

_InterfaceTypeExtension_Sequence_name = (Core.Name "name")

_InterfaceTypeExtension_Sequence_implementsInterfaces = (Core.Name "implementsInterfaces")

_InterfaceTypeExtension_Sequence_directives = (Core.Name "directives")

_InterfaceTypeExtension_Sequence_fieldsDefinition = (Core.Name "fieldsDefinition")

_InterfaceTypeExtension_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = (Core.TypeOptional _ImplementsInterfaces_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fieldsDefinition"),
      Core.fieldTypeType = _FieldsDefinition_type_}]}))

data InterfaceTypeExtension_Sequence2 = 
  InterfaceTypeExtension_Sequence2 {
    interfaceTypeExtension_Sequence2Name :: Name,
    interfaceTypeExtension_Sequence2ImplementsInterfaces :: (Maybe ImplementsInterfaces),
    interfaceTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.InterfaceTypeExtension.Sequence2")

_InterfaceTypeExtension_Sequence2_name = (Core.Name "name")

_InterfaceTypeExtension_Sequence2_implementsInterfaces = (Core.Name "implementsInterfaces")

_InterfaceTypeExtension_Sequence2_directives = (Core.Name "directives")

_InterfaceTypeExtension_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = (Core.TypeOptional _ImplementsInterfaces_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = _Directives_type_}]}))

data InterfaceTypeExtension_Sequence3 = 
  InterfaceTypeExtension_Sequence3 {
    interfaceTypeExtension_Sequence3Name :: Name,
    interfaceTypeExtension_Sequence3ImplementsInterfaces :: ImplementsInterfaces}
  deriving (Eq, Ord, Read, Show)

_InterfaceTypeExtension_Sequence3 = (Core.Name "hydra/langs/graphql/syntax.InterfaceTypeExtension.Sequence3")

_InterfaceTypeExtension_Sequence3_name = (Core.Name "name")

_InterfaceTypeExtension_Sequence3_implementsInterfaces = (Core.Name "implementsInterfaces")

_InterfaceTypeExtension_Sequence3_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implementsInterfaces"),
      Core.fieldTypeType = _ImplementsInterfaces_type_}]}))

data UnionTypeDefinition = 
  UnionTypeDefinition {
    unionTypeDefinitionDescription :: (Maybe Description),
    unionTypeDefinitionName :: Name,
    unionTypeDefinitionDirectives :: (Maybe Directives),
    unionTypeDefinitionUnionMemberTypes :: (Maybe UnionMemberTypes)}
  deriving (Eq, Ord, Read, Show)

_UnionTypeDefinition = (Core.Name "hydra/langs/graphql/syntax.UnionTypeDefinition")

_UnionTypeDefinition_description = (Core.Name "description")

_UnionTypeDefinition_name = (Core.Name "name")

_UnionTypeDefinition_directives = (Core.Name "directives")

_UnionTypeDefinition_unionMemberTypes = (Core.Name "unionMemberTypes")

_UnionTypeDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unionMemberTypes"),
      Core.fieldTypeType = (Core.TypeOptional _UnionMemberTypes_type_)}]}))

data UnionMemberTypes = 
  UnionMemberTypesSequence UnionMemberTypes_Sequence |
  UnionMemberTypesSequence2 UnionMemberTypes_Sequence2
  deriving (Eq, Ord, Read, Show)

_UnionMemberTypes = (Core.Name "hydra/langs/graphql/syntax.UnionMemberTypes")

_UnionMemberTypes_sequence = (Core.Name "sequence")

_UnionMemberTypes_sequence2 = (Core.Name "sequence2")

_UnionMemberTypes_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _UnionMemberTypes_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _UnionMemberTypes_Sequence2_type_}]}))

data UnionMemberTypes_Sequence = 
  UnionMemberTypes_Sequence {
    unionMemberTypes_SequenceUnionMemberTypes :: UnionMemberTypes,
    unionMemberTypes_SequenceNamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_UnionMemberTypes_Sequence = (Core.Name "hydra/langs/graphql/syntax.UnionMemberTypes.Sequence")

_UnionMemberTypes_Sequence_unionMemberTypes = (Core.Name "unionMemberTypes")

_UnionMemberTypes_Sequence_namedType = (Core.Name "namedType")

_UnionMemberTypes_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unionMemberTypes"),
      Core.fieldTypeType = _UnionMemberTypes_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namedType"),
      Core.fieldTypeType = _NamedType_type_}]}))

data UnionMemberTypes_Sequence2 = 
  UnionMemberTypes_Sequence2 {
    unionMemberTypes_Sequence2Or :: (Maybe ()),
    unionMemberTypes_Sequence2NamedType :: NamedType}
  deriving (Eq, Ord, Read, Show)

_UnionMemberTypes_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.UnionMemberTypes.Sequence2")

_UnionMemberTypes_Sequence2_or = (Core.Name "or")

_UnionMemberTypes_Sequence2_namedType = (Core.Name "namedType")

_UnionMemberTypes_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namedType"),
      Core.fieldTypeType = _NamedType_type_}]}))

data UnionTypeExtension = 
  UnionTypeExtensionSequence UnionTypeExtension_Sequence |
  UnionTypeExtensionSequence2 UnionTypeExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_UnionTypeExtension = (Core.Name "hydra/langs/graphql/syntax.UnionTypeExtension")

_UnionTypeExtension_sequence = (Core.Name "sequence")

_UnionTypeExtension_sequence2 = (Core.Name "sequence2")

_UnionTypeExtension_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _UnionTypeExtension_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _UnionTypeExtension_Sequence2_type_}]}))

data UnionTypeExtension_Sequence = 
  UnionTypeExtension_Sequence {
    unionTypeExtension_SequenceName :: Name,
    unionTypeExtension_SequenceDirectives :: (Maybe Directives),
    unionTypeExtension_SequenceUnionMemberTypes :: UnionMemberTypes}
  deriving (Eq, Ord, Read, Show)

_UnionTypeExtension_Sequence = (Core.Name "hydra/langs/graphql/syntax.UnionTypeExtension.Sequence")

_UnionTypeExtension_Sequence_name = (Core.Name "name")

_UnionTypeExtension_Sequence_directives = (Core.Name "directives")

_UnionTypeExtension_Sequence_unionMemberTypes = (Core.Name "unionMemberTypes")

_UnionTypeExtension_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unionMemberTypes"),
      Core.fieldTypeType = _UnionMemberTypes_type_}]}))

data UnionTypeExtension_Sequence2 = 
  UnionTypeExtension_Sequence2 {
    unionTypeExtension_Sequence2Name :: Name,
    unionTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_UnionTypeExtension_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.UnionTypeExtension.Sequence2")

_UnionTypeExtension_Sequence2_name = (Core.Name "name")

_UnionTypeExtension_Sequence2_directives = (Core.Name "directives")

_UnionTypeExtension_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = _Directives_type_}]}))

data EnumTypeDefinition = 
  EnumTypeDefinition {
    enumTypeDefinitionDescription :: (Maybe Description),
    enumTypeDefinitionName :: Name,
    enumTypeDefinitionDirectives :: (Maybe Directives),
    enumTypeDefinitionEnumValuesDefinition :: (Maybe EnumValuesDefinition)}
  deriving (Eq, Ord, Read, Show)

_EnumTypeDefinition = (Core.Name "hydra/langs/graphql/syntax.EnumTypeDefinition")

_EnumTypeDefinition_description = (Core.Name "description")

_EnumTypeDefinition_name = (Core.Name "name")

_EnumTypeDefinition_directives = (Core.Name "directives")

_EnumTypeDefinition_enumValuesDefinition = (Core.Name "enumValuesDefinition")

_EnumTypeDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enumValuesDefinition"),
      Core.fieldTypeType = (Core.TypeOptional _EnumValuesDefinition_type_)}]}))

newtype EnumValuesDefinition = 
  EnumValuesDefinition {
    unEnumValuesDefinition :: [EnumValueDefinition]}
  deriving (Eq, Ord, Read, Show)

_EnumValuesDefinition = (Core.Name "hydra/langs/graphql/syntax.EnumValuesDefinition")

_EnumValuesDefinition_type_ = (Core.TypeList _EnumValueDefinition_type_)

data EnumValueDefinition = 
  EnumValueDefinition {
    enumValueDefinitionDescription :: (Maybe Description),
    enumValueDefinitionEnumValue :: EnumValue,
    enumValueDefinitionDirectives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_EnumValueDefinition = (Core.Name "hydra/langs/graphql/syntax.EnumValueDefinition")

_EnumValueDefinition_description = (Core.Name "description")

_EnumValueDefinition_enumValue = (Core.Name "enumValue")

_EnumValueDefinition_directives = (Core.Name "directives")

_EnumValueDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enumValue"),
      Core.fieldTypeType = _EnumValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)}]}))

data EnumTypeExtension = 
  EnumTypeExtensionSequence EnumTypeExtension_Sequence |
  EnumTypeExtensionSequence2 EnumTypeExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_EnumTypeExtension = (Core.Name "hydra/langs/graphql/syntax.EnumTypeExtension")

_EnumTypeExtension_sequence = (Core.Name "sequence")

_EnumTypeExtension_sequence2 = (Core.Name "sequence2")

_EnumTypeExtension_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _EnumTypeExtension_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _EnumTypeExtension_Sequence2_type_}]}))

data EnumTypeExtension_Sequence = 
  EnumTypeExtension_Sequence {
    enumTypeExtension_SequenceName :: Name,
    enumTypeExtension_SequenceDirectives :: (Maybe Directives),
    enumTypeExtension_SequenceEnumValuesDefinition :: EnumValuesDefinition}
  deriving (Eq, Ord, Read, Show)

_EnumTypeExtension_Sequence = (Core.Name "hydra/langs/graphql/syntax.EnumTypeExtension.Sequence")

_EnumTypeExtension_Sequence_name = (Core.Name "name")

_EnumTypeExtension_Sequence_directives = (Core.Name "directives")

_EnumTypeExtension_Sequence_enumValuesDefinition = (Core.Name "enumValuesDefinition")

_EnumTypeExtension_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enumValuesDefinition"),
      Core.fieldTypeType = _EnumValuesDefinition_type_}]}))

data EnumTypeExtension_Sequence2 = 
  EnumTypeExtension_Sequence2 {
    enumTypeExtension_Sequence2Name :: Name,
    enumTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_EnumTypeExtension_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.EnumTypeExtension.Sequence2")

_EnumTypeExtension_Sequence2_name = (Core.Name "name")

_EnumTypeExtension_Sequence2_directives = (Core.Name "directives")

_EnumTypeExtension_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = _Directives_type_}]}))

data InputObjectTypeDefinition = 
  InputObjectTypeDefinitionSequence InputObjectTypeDefinition_Sequence |
  InputObjectTypeDefinitionSequence2 InputObjectTypeDefinition_Sequence2
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeDefinition = (Core.Name "hydra/langs/graphql/syntax.InputObjectTypeDefinition")

_InputObjectTypeDefinition_sequence = (Core.Name "sequence")

_InputObjectTypeDefinition_sequence2 = (Core.Name "sequence2")

_InputObjectTypeDefinition_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _InputObjectTypeDefinition_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _InputObjectTypeDefinition_Sequence2_type_}]}))

data InputObjectTypeDefinition_Sequence = 
  InputObjectTypeDefinition_Sequence {
    inputObjectTypeDefinition_SequenceDescription :: (Maybe Description),
    inputObjectTypeDefinition_SequenceName :: Name,
    inputObjectTypeDefinition_SequenceDirectives :: (Maybe Directives),
    inputObjectTypeDefinition_SequenceInputFieldsDefinition :: InputFieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeDefinition_Sequence = (Core.Name "hydra/langs/graphql/syntax.InputObjectTypeDefinition.Sequence")

_InputObjectTypeDefinition_Sequence_description = (Core.Name "description")

_InputObjectTypeDefinition_Sequence_name = (Core.Name "name")

_InputObjectTypeDefinition_Sequence_directives = (Core.Name "directives")

_InputObjectTypeDefinition_Sequence_inputFieldsDefinition = (Core.Name "inputFieldsDefinition")

_InputObjectTypeDefinition_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inputFieldsDefinition"),
      Core.fieldTypeType = _InputFieldsDefinition_type_}]}))

data InputObjectTypeDefinition_Sequence2 = 
  InputObjectTypeDefinition_Sequence2 {
    inputObjectTypeDefinition_Sequence2Description :: (Maybe Description),
    inputObjectTypeDefinition_Sequence2Name :: Name,
    inputObjectTypeDefinition_Sequence2Directives :: (Maybe Directives)}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeDefinition_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.InputObjectTypeDefinition.Sequence2")

_InputObjectTypeDefinition_Sequence2_description = (Core.Name "description")

_InputObjectTypeDefinition_Sequence2_name = (Core.Name "name")

_InputObjectTypeDefinition_Sequence2_directives = (Core.Name "directives")

_InputObjectTypeDefinition_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)}]}))

newtype InputFieldsDefinition = 
  InputFieldsDefinition {
    unInputFieldsDefinition :: [InputValueDefinition]}
  deriving (Eq, Ord, Read, Show)

_InputFieldsDefinition = (Core.Name "hydra/langs/graphql/syntax.InputFieldsDefinition")

_InputFieldsDefinition_type_ = (Core.TypeList _InputValueDefinition_type_)

data InputObjectTypeExtension = 
  InputObjectTypeExtensionSequence InputObjectTypeExtension_Sequence |
  InputObjectTypeExtensionSequence2 InputObjectTypeExtension_Sequence2
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeExtension = (Core.Name "hydra/langs/graphql/syntax.InputObjectTypeExtension")

_InputObjectTypeExtension_sequence = (Core.Name "sequence")

_InputObjectTypeExtension_sequence2 = (Core.Name "sequence2")

_InputObjectTypeExtension_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _InputObjectTypeExtension_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _InputObjectTypeExtension_Sequence2_type_}]}))

data InputObjectTypeExtension_Sequence = 
  InputObjectTypeExtension_Sequence {
    inputObjectTypeExtension_SequenceName :: Name,
    inputObjectTypeExtension_SequenceDirectives :: (Maybe Directives),
    inputObjectTypeExtension_SequenceInputFieldsDefinition :: InputFieldsDefinition}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeExtension_Sequence = (Core.Name "hydra/langs/graphql/syntax.InputObjectTypeExtension.Sequence")

_InputObjectTypeExtension_Sequence_name = (Core.Name "name")

_InputObjectTypeExtension_Sequence_directives = (Core.Name "directives")

_InputObjectTypeExtension_Sequence_inputFieldsDefinition = (Core.Name "inputFieldsDefinition")

_InputObjectTypeExtension_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = (Core.TypeOptional _Directives_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inputFieldsDefinition"),
      Core.fieldTypeType = _InputFieldsDefinition_type_}]}))

data InputObjectTypeExtension_Sequence2 = 
  InputObjectTypeExtension_Sequence2 {
    inputObjectTypeExtension_Sequence2Name :: Name,
    inputObjectTypeExtension_Sequence2Directives :: Directives}
  deriving (Eq, Ord, Read, Show)

_InputObjectTypeExtension_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.InputObjectTypeExtension.Sequence2")

_InputObjectTypeExtension_Sequence2_name = (Core.Name "name")

_InputObjectTypeExtension_Sequence2_directives = (Core.Name "directives")

_InputObjectTypeExtension_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directives"),
      Core.fieldTypeType = _Directives_type_}]}))

data DirectiveDefinition = 
  DirectiveDefinition {
    directiveDefinitionDescription :: (Maybe Description),
    directiveDefinitionName :: Name,
    directiveDefinitionArgumentsDefinition :: (Maybe ArgumentsDefinition),
    directiveDefinitionRepeatable :: (Maybe ()),
    directiveDefinitionDirectiveLocations :: DirectiveLocations}
  deriving (Eq, Ord, Read, Show)

_DirectiveDefinition = (Core.Name "hydra/langs/graphql/syntax.DirectiveDefinition")

_DirectiveDefinition_description = (Core.Name "description")

_DirectiveDefinition_name = (Core.Name "name")

_DirectiveDefinition_argumentsDefinition = (Core.Name "argumentsDefinition")

_DirectiveDefinition_repeatable = (Core.Name "repeatable")

_DirectiveDefinition_directiveLocations = (Core.Name "directiveLocations")

_DirectiveDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = (Core.TypeOptional _Description_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _Name_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "argumentsDefinition"),
      Core.fieldTypeType = (Core.TypeOptional _ArgumentsDefinition_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "repeatable"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directiveLocations"),
      Core.fieldTypeType = _DirectiveLocations_type_}]}))

data DirectiveLocations = 
  DirectiveLocationsSequence DirectiveLocations_Sequence |
  DirectiveLocationsSequence2 DirectiveLocations_Sequence2
  deriving (Eq, Ord, Read, Show)

_DirectiveLocations = (Core.Name "hydra/langs/graphql/syntax.DirectiveLocations")

_DirectiveLocations_sequence = (Core.Name "sequence")

_DirectiveLocations_sequence2 = (Core.Name "sequence2")

_DirectiveLocations_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = _DirectiveLocations_Sequence_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence2"),
      Core.fieldTypeType = _DirectiveLocations_Sequence2_type_}]}))

data DirectiveLocations_Sequence = 
  DirectiveLocations_Sequence {
    directiveLocations_SequenceDirectiveLocations :: DirectiveLocations,
    directiveLocations_SequenceDirectiveLocation :: DirectiveLocation}
  deriving (Eq, Ord, Read, Show)

_DirectiveLocations_Sequence = (Core.Name "hydra/langs/graphql/syntax.DirectiveLocations.Sequence")

_DirectiveLocations_Sequence_directiveLocations = (Core.Name "directiveLocations")

_DirectiveLocations_Sequence_directiveLocation = (Core.Name "directiveLocation")

_DirectiveLocations_Sequence_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directiveLocations"),
      Core.fieldTypeType = _DirectiveLocations_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directiveLocation"),
      Core.fieldTypeType = _DirectiveLocation_type_}]}))

data DirectiveLocations_Sequence2 = 
  DirectiveLocations_Sequence2 {
    directiveLocations_Sequence2Or :: (Maybe ()),
    directiveLocations_Sequence2DirectiveLocation :: DirectiveLocation}
  deriving (Eq, Ord, Read, Show)

_DirectiveLocations_Sequence2 = (Core.Name "hydra/langs/graphql/syntax.DirectiveLocations.Sequence2")

_DirectiveLocations_Sequence2_or = (Core.Name "or")

_DirectiveLocations_Sequence2_directiveLocation = (Core.Name "directiveLocation")

_DirectiveLocations_Sequence2_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directiveLocation"),
      Core.fieldTypeType = _DirectiveLocation_type_}]}))

data DirectiveLocation = 
  DirectiveLocationExecutable ExecutableDirectiveLocation |
  DirectiveLocationTypeSystem TypeSystemDirectiveLocation
  deriving (Eq, Ord, Read, Show)

_DirectiveLocation = (Core.Name "hydra/langs/graphql/syntax.DirectiveLocation")

_DirectiveLocation_executable = (Core.Name "executable")

_DirectiveLocation_typeSystem = (Core.Name "typeSystem")

_DirectiveLocation_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "executable"),
      Core.fieldTypeType = _ExecutableDirectiveLocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeSystem"),
      Core.fieldTypeType = _TypeSystemDirectiveLocation_type_}]}))

data ExecutableDirectiveLocation = 
  ExecutableDirectiveLocationQUERY  |
  ExecutableDirectiveLocationMUTATION  |
  ExecutableDirectiveLocationSUBSCRIPTION  |
  ExecutableDirectiveLocationFIELD  |
  ExecutableDirectiveLocationFRAGMENTLowbarDEFINITION  |
  ExecutableDirectiveLocationFRAGMENTLowbarSPREAD  |
  ExecutableDirectiveLocationINLINELowbarFRAGMENT  |
  ExecutableDirectiveLocationVARIABLELowbarDEFINITION 
  deriving (Eq, Ord, Read, Show)

_ExecutableDirectiveLocation = (Core.Name "hydra/langs/graphql/syntax.ExecutableDirectiveLocation")

_ExecutableDirectiveLocation_qUERY = (Core.Name "qUERY")

_ExecutableDirectiveLocation_mUTATION = (Core.Name "mUTATION")

_ExecutableDirectiveLocation_sUBSCRIPTION = (Core.Name "sUBSCRIPTION")

_ExecutableDirectiveLocation_fIELD = (Core.Name "fIELD")

_ExecutableDirectiveLocation_fRAGMENTLowbarDEFINITION = (Core.Name "fRAGMENTLowbarDEFINITION")

_ExecutableDirectiveLocation_fRAGMENTLowbarSPREAD = (Core.Name "fRAGMENTLowbarSPREAD")

_ExecutableDirectiveLocation_iNLINELowbarFRAGMENT = (Core.Name "iNLINELowbarFRAGMENT")

_ExecutableDirectiveLocation_vARIABLELowbarDEFINITION = (Core.Name "vARIABLELowbarDEFINITION")

_ExecutableDirectiveLocation_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qUERY"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mUTATION"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sUBSCRIPTION"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fIELD"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fRAGMENTLowbarDEFINITION"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fRAGMENTLowbarSPREAD"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iNLINELowbarFRAGMENT"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "vARIABLELowbarDEFINITION"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TypeSystemDirectiveLocation = 
  TypeSystemDirectiveLocationSCHEMA  |
  TypeSystemDirectiveLocationSCALAR  |
  TypeSystemDirectiveLocationOBJECT  |
  TypeSystemDirectiveLocationFIELDLowbarDEFINITION  |
  TypeSystemDirectiveLocationARGUMENTLowbarDEFINITION  |
  TypeSystemDirectiveLocationINTERFACE  |
  TypeSystemDirectiveLocationUNION  |
  TypeSystemDirectiveLocationENUM  |
  TypeSystemDirectiveLocationENUMLowbarVALUE  |
  TypeSystemDirectiveLocationINPUTLowbarOBJECT  |
  TypeSystemDirectiveLocationINPUTLowbarFIELDLowbarDEFINITION 
  deriving (Eq, Ord, Read, Show)

_TypeSystemDirectiveLocation = (Core.Name "hydra/langs/graphql/syntax.TypeSystemDirectiveLocation")

_TypeSystemDirectiveLocation_sCHEMA = (Core.Name "sCHEMA")

_TypeSystemDirectiveLocation_sCALAR = (Core.Name "sCALAR")

_TypeSystemDirectiveLocation_oBJECT = (Core.Name "oBJECT")

_TypeSystemDirectiveLocation_fIELDLowbarDEFINITION = (Core.Name "fIELDLowbarDEFINITION")

_TypeSystemDirectiveLocation_aRGUMENTLowbarDEFINITION = (Core.Name "aRGUMENTLowbarDEFINITION")

_TypeSystemDirectiveLocation_iNTERFACE = (Core.Name "iNTERFACE")

_TypeSystemDirectiveLocation_uNION = (Core.Name "uNION")

_TypeSystemDirectiveLocation_eNUM = (Core.Name "eNUM")

_TypeSystemDirectiveLocation_eNUMLowbarVALUE = (Core.Name "eNUMLowbarVALUE")

_TypeSystemDirectiveLocation_iNPUTLowbarOBJECT = (Core.Name "iNPUTLowbarOBJECT")

_TypeSystemDirectiveLocation_iNPUTLowbarFIELDLowbarDEFINITION = (Core.Name "iNPUTLowbarFIELDLowbarDEFINITION")

_TypeSystemDirectiveLocation_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sCHEMA"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sCALAR"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "oBJECT"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fIELDLowbarDEFINITION"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "aRGUMENTLowbarDEFINITION"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iNTERFACE"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "uNION"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "eNUM"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "eNUMLowbarVALUE"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iNPUTLowbarOBJECT"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iNPUTLowbarFIELDLowbarDEFINITION"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))