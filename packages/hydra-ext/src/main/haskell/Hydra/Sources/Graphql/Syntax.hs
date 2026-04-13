module Hydra.Sources.Graphql.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T


ns :: Namespace
ns = Namespace "hydra.graphql.syntax"

define :: String -> Type -> Binding
define = defineType ns

gql :: String -> Type
gql = typeref ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [] [] $
    Just ("A GraphQL model. Based on the (extended) BNF at:\n" ++
      "  https://spec.graphql.org/draft/#sec-Appendix-Grammar-Summary")
  where
    definitions = [
      name_,
      intValue,
      floatValue,
      stringValue,
      document,
      definition,
      executableDocument,
      executableDefinition,
      operationDefinition,
      operationDefinition_Sequence,
      operationType,
      selectionSet,
      selection,
      field_,
      alias_,
      arguments,
      argument,
      fragmentSpread,
      inlineFragment,
      fragmentDefinition,
      fragmentName,
      typeCondition,
      value,
      booleanValue,
      nullValue,
      enumValue,
      listValue,
      listValue_Sequence,
      objectValue,
      objectValue_Sequence,
      objectField,
      variablesDefinition,
      variable,
      defaultValue,
      type_,
      namedType,
      listType,
      nonNullType,
      directives,
      directive,
      typeSystemDocment,
      typeSystemDefinition,
      typeSystemExtensionDocument,
      typeSystemDefinitionOrExtension,
      typeSystemExtension,
      schemaDefinition,
      schemaExtension,
      schemaExtension_Sequence,
      rootOperationTypeDefinition,
      description,
      typeDefinition,
      typeExtension,
      scalarTypeDefinition,
      scalarTypeExtension,
      objectTypeDefinition,
      objectTypeExtension,
      objectTypeExtension_Sequence,
      objectTypeExtension_Sequence2,
      objectTypeExtension_Sequence3,
      implementsInterfaces,
      implementsInterfaces_Sequence,
      implementsInterfaces_Sequence2,
      fieldsDefinition,
      fieldDefinition,
      argumentsDefinition,
      inputValueDefinition,
      interfaceTypeDefinition,
      interfaceTypeDefinition_Sequence,
      interfaceTypeDefinition_Sequence2,
      interfaceTypeExtension,
      interfaceTypeExtension_Sequence,
      interfaceTypeExtension_Sequence2,
      interfaceTypeExtension_Sequence3,
      unionTypeDefinition,
      unionMemberTypes,
      unionMemberTypes_Sequence,
      unionMemberTypes_Sequence2,
      unionTypeExtension,
      unionTypeExtension_Sequence,
      unionTypeExtension_Sequence2,
      enumTypeDefinition,
      enumValuesDefinition,
      enumValueDefinition,
      enumTypeExtension,
      enumTypeExtension_Sequence,
      enumTypeExtension_Sequence2,
      inputObjectTypeDefinition,
      inputObjectTypeDefinition_Sequence,
      inputObjectTypeDefinition_Sequence2,
      inputFieldsDefinition,
      inputObjectTypeExtension,
      inputObjectTypeExtension_Sequence,
      inputObjectTypeExtension_Sequence2,
      directiveDefinition,
      directiveLocations,
      directiveLocations_Sequence,
      directiveLocations_Sequence2,
      directiveLocation,
      executableDirectiveLocation,
      typeSystemDirectiveLocation]

-- Token definitions

name_ :: Binding
name_ = define "Name" $ T.wrap T.string

intValue :: Binding
intValue = define "IntValue" $ T.wrap T.string

floatValue :: Binding
floatValue = define "FloatValue" $ T.wrap T.string

stringValue :: Binding
stringValue = define "StringValue" $ T.wrap T.string

-- Document definitions

document :: Binding
document = define "Document" $ T.wrap $ T.list $ gql "Definition"

definition :: Binding
definition = define "Definition" $
  T.union [
    "executable">: gql "ExecutableDefinition",
    "typeSystem">: gql "TypeSystemDefinitionOrExtension"]

executableDocument :: Binding
executableDocument = define "ExecutableDocument" $ T.wrap $ T.list $ gql "ExecutableDefinition"

executableDefinition :: Binding
executableDefinition = define "ExecutableDefinition" $
  T.union [
    "operation">: gql "OperationDefinition",
    "fragment">: gql "FragmentDefinition"]

operationDefinition :: Binding
operationDefinition = define "OperationDefinition" $
  T.union [
    "sequence">: gql "OperationDefinition_Sequence",
    "SelectionSet">: gql "SelectionSet"]

operationDefinition_Sequence :: Binding
operationDefinition_Sequence = define "OperationDefinition_Sequence" $
  T.record [
    "OperationType">: gql "OperationType",
    "Name">: T.maybe $ gql "Name",
    "VariablesDefinition">: T.maybe $ gql "VariablesDefinition",
    "Directives">: T.maybe $ gql "Directives",
    "SelectionSet">: gql "SelectionSet"]

operationType :: Binding
operationType = define "OperationType" $
  T.union [
    "Query">: T.unit,
    "Mutation">: T.unit,
    "Subscription">: T.unit]

selectionSet :: Binding
selectionSet = define "SelectionSet" $ T.wrap $ T.list $ gql "Selection"

selection :: Binding
selection = define "Selection" $
  T.union [
    "Field">: gql "Field",
    "FragmentSpread">: gql "FragmentSpread",
    "InlineFragment">: gql "InlineFragment"]

field_ :: Binding
field_ = define "Field" $
  T.record [
    "Alias">: T.maybe $ gql "Alias",
    "Name">: gql "Name",
    "Arguments">: T.maybe $ gql "Arguments",
    "Directives">: T.maybe $ gql "Directives",
    "SelectionSet">: T.maybe $ gql "SelectionSet"]

alias_ :: Binding
alias_ = define "Alias" $
  T.union [
    "Name">: gql "Name",
    "Colon">: T.unit]

arguments :: Binding
arguments = define "Arguments" $ T.wrap $ T.list $ gql "Argument"

argument :: Binding
argument = define "Argument" $
  T.record [
    "Name">: gql "Name",
    "Value">: gql "Value"]

fragmentSpread :: Binding
fragmentSpread = define "FragmentSpread" $
  T.record [
    "FragmentName">: gql "FragmentName",
    "Directives">: T.maybe $ gql "Directives"]

inlineFragment :: Binding
inlineFragment = define "InlineFragment" $
  T.record [
    "TypeCondition">: T.maybe $ gql "TypeCondition",
    "Directives">: T.maybe $ gql "Directives",
    "SelectionSet">: gql "SelectionSet"]

fragmentDefinition :: Binding
fragmentDefinition = define "FragmentDefinition" $
  T.record [
    "FragmentName">: gql "FragmentName",
    "TypeCondition">: gql "TypeCondition",
    "Directives">: T.maybe $ gql "Directives",
    "SelectionSet">: gql "SelectionSet"]

fragmentName :: Binding
fragmentName = define "FragmentName" $ T.wrap $ gql "Name"

typeCondition :: Binding
typeCondition = define "TypeCondition" $
  T.union [
    "On">: T.unit,
    "NamedType">: gql "NamedType"]

value :: Binding
value = define "Value" $
  T.union [
    "Variable">: gql "Variable",
    "int">: gql "IntValue",
    "float">: gql "FloatValue",
    "string">: gql "StringValue",
    "boolean">: gql "BooleanValue",
    "null">: gql "NullValue",
    "enum">: gql "EnumValue",
    "list">: gql "ListValue",
    "object">: gql "ObjectValue"]

booleanValue :: Binding
booleanValue = define "BooleanValue" $
  T.union [
    "True">: T.unit,
    "False">: T.unit]

nullValue :: Binding
nullValue = define "NullValue" $ T.wrap T.unit

enumValue :: Binding
enumValue = define "EnumValue" $ T.wrap $ gql "Name"

listValue :: Binding
listValue = define "ListValue" $
  T.union [
    "sequence">: gql "ListValue_Sequence",
    "sequence2">: T.list $ gql "Value"]

listValue_Sequence :: Binding
listValue_Sequence = define "ListValue_Sequence" $ T.record []

objectValue :: Binding
objectValue = define "ObjectValue" $
  T.union [
    "sequence">: gql "ObjectValue_Sequence",
    "sequence2">: T.list $ gql "ObjectField"]

objectValue_Sequence :: Binding
objectValue_Sequence = define "ObjectValue_Sequence" $ T.record []

objectField :: Binding
objectField = define "ObjectField" $
  T.record [
    "Name">: gql "Name",
    "Value">: gql "Value"]

variablesDefinition :: Binding
variablesDefinition = define "VariablesDefinition" $
  T.record [
    "Variable">: gql "Variable",
    "Type">: gql "Type",
    "DefaultValue">: T.maybe $ gql "DefaultValue",
    "Directives">: T.maybe $ gql "Directives"]

variable :: Binding
variable = define "Variable" $ T.wrap $ gql "Name"

defaultValue :: Binding
defaultValue = define "DefaultValue" $ T.wrap $ gql "Value"

type_ :: Binding
type_ = define "Type" $
  T.union [
    "named">: gql "NamedType",
    "list">: gql "ListType",
    "nonNull">: gql "NonNullType"]

namedType :: Binding
namedType = define "NamedType" $ T.wrap $ gql "Name"

listType :: Binding
listType = define "ListType" $ T.wrap $ gql "Type"

nonNullType :: Binding
nonNullType = define "NonNullType" $
  T.union [
    "named">: gql "NamedType",
    "list">: gql "ListType"]

directives :: Binding
directives = define "Directives" $ T.wrap $ T.list $ gql "Directive"

directive :: Binding
directive = define "Directive" $
  T.record [
    "Name">: gql "Name",
    "Arguments">: T.maybe $ gql "Arguments"]

-- Type system definitions

typeSystemDocment :: Binding
typeSystemDocment = define "TypeSystemDocment" $ T.wrap $ T.list $ gql "TypeSystemDefinition"

typeSystemDefinition :: Binding
typeSystemDefinition = define "TypeSystemDefinition" $
  T.union [
    "schema">: gql "SchemaDefinition",
    "type">: gql "TypeDefinition",
    "directive">: gql "DirectiveDefinition"]

typeSystemExtensionDocument :: Binding
typeSystemExtensionDocument = define "TypeSystemExtensionDocument" $
  T.wrap $ T.list $ gql "TypeSystemDefinitionOrExtension"

typeSystemDefinitionOrExtension :: Binding
typeSystemDefinitionOrExtension = define "TypeSystemDefinitionOrExtension" $
  T.union [
    "definition">: gql "TypeSystemDefinition",
    "extension">: gql "TypeSystemExtension"]

typeSystemExtension :: Binding
typeSystemExtension = define "TypeSystemExtension" $
  T.union [
    "schema">: gql "SchemaExtension",
    "type">: gql "TypeExtension"]

schemaDefinition :: Binding
schemaDefinition = define "SchemaDefinition" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Directives">: T.maybe $ gql "Directives",
    "RootOperationTypeDefinition">: gql "RootOperationTypeDefinition"]

schemaExtension :: Binding
schemaExtension = define "SchemaExtension" $
  T.union [
    "sequence">: gql "SchemaExtension_Sequence",
    "sequence2">: gql "Directives"]

schemaExtension_Sequence :: Binding
schemaExtension_Sequence = define "SchemaExtension_Sequence" $
  T.record [
    "Directives">: T.maybe $ gql "Directives",
    "RootOperationTypeDefinition">: gql "RootOperationTypeDefinition"]

rootOperationTypeDefinition :: Binding
rootOperationTypeDefinition = define "RootOperationTypeDefinition" $
  T.record [
    "OperationType">: gql "OperationType",
    "NamedType">: gql "NamedType"]

description :: Binding
description = define "Description" $ T.wrap $ gql "StringValue"

typeDefinition :: Binding
typeDefinition = define "TypeDefinition" $
  T.union [
    "scalar">: gql "ScalarTypeDefinition",
    "object">: gql "ObjectTypeDefinition",
    "interface">: gql "InterfaceTypeDefinition",
    "union">: gql "UnionTypeDefinition",
    "enum">: gql "EnumTypeDefinition",
    "inputObject">: gql "InputObjectTypeDefinition"]

typeExtension :: Binding
typeExtension = define "TypeExtension" $
  T.union [
    "scalar">: gql "ScalarTypeExtension",
    "object">: gql "ObjectTypeExtension",
    "interface">: gql "InterfaceTypeExtension",
    "union">: gql "UnionTypeExtension",
    "enum">: gql "EnumTypeExtension",
    "inputObject">: gql "InputObjectTypeExtension"]

scalarTypeDefinition :: Binding
scalarTypeDefinition = define "ScalarTypeDefinition" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.maybe $ gql "Directives"]

scalarTypeExtension :: Binding
scalarTypeExtension = define "ScalarTypeExtension" $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

objectTypeDefinition :: Binding
objectTypeDefinition = define "ObjectTypeDefinition" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.maybe $ gql "ImplementsInterfaces",
    "Directives">: T.maybe $ gql "Directives",
    "FieldsDefinition">: T.maybe $ gql "FieldsDefinition"]

objectTypeExtension :: Binding
objectTypeExtension = define "ObjectTypeExtension" $
  T.union [
    "sequence">: gql "ObjectTypeExtension_Sequence",
    "sequence2">: gql "ObjectTypeExtension_Sequence2",
    "sequence3">: gql "ObjectTypeExtension_Sequence3"]

objectTypeExtension_Sequence :: Binding
objectTypeExtension_Sequence = define "ObjectTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.maybe $ gql "ImplementsInterfaces",
    "Directives">: T.maybe $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

objectTypeExtension_Sequence2 :: Binding
objectTypeExtension_Sequence2 = define "ObjectTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.maybe $ gql "ImplementsInterfaces",
    "Directives">: T.maybe $ gql "Directives"]

objectTypeExtension_Sequence3 :: Binding
objectTypeExtension_Sequence3 = define "ObjectTypeExtension_Sequence3" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces"]

implementsInterfaces :: Binding
implementsInterfaces = define "ImplementsInterfaces" $
  T.union [
    "sequence">: gql "ImplementsInterfaces_Sequence",
    "sequence2">: gql "ImplementsInterfaces_Sequence2"]

implementsInterfaces_Sequence :: Binding
implementsInterfaces_Sequence = define "ImplementsInterfaces_Sequence" $
  T.record [
    "ImplementsInterfaces">: gql "ImplementsInterfaces",
    "NamedType">: gql "NamedType"]

implementsInterfaces_Sequence2 :: Binding
implementsInterfaces_Sequence2 = define "ImplementsInterfaces_Sequence2" $
  T.record [
    "Amp">: T.maybe T.unit,
    "NamedType">: gql "NamedType"]

fieldsDefinition :: Binding
fieldsDefinition = define "FieldsDefinition" $ T.wrap $ T.list $ gql "FieldDefinition"

fieldDefinition :: Binding
fieldDefinition = define "FieldDefinition" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "ArgumentsDefinition">: T.maybe $ gql "ArgumentsDefinition",
    "Type">: gql "Type",
    "Directives">: T.maybe $ gql "Directives"]

argumentsDefinition :: Binding
argumentsDefinition = define "ArgumentsDefinition" $ T.wrap $ T.list $ gql "InputValueDefinition"

inputValueDefinition :: Binding
inputValueDefinition = define "InputValueDefinition" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "Type">: gql "Type",
    "DefaultValue">: T.maybe $ gql "DefaultValue",
    "Directives">: T.maybe $ gql "Directives"]

interfaceTypeDefinition :: Binding
interfaceTypeDefinition = define "InterfaceTypeDefinition" $
  T.union [
    "sequence">: gql "InterfaceTypeDefinition_Sequence",
    "sequence2">: gql "InterfaceTypeDefinition_Sequence2"]

interfaceTypeDefinition_Sequence :: Binding
interfaceTypeDefinition_Sequence = define "InterfaceTypeDefinition_Sequence" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.maybe $ gql "ImplementsInterfaces",
    "Directives">: T.maybe $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

interfaceTypeDefinition_Sequence2 :: Binding
interfaceTypeDefinition_Sequence2 = define "InterfaceTypeDefinition_Sequence2" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces",
    "Directives">: T.maybe $ gql "Directives"]

interfaceTypeExtension :: Binding
interfaceTypeExtension = define "InterfaceTypeExtension" $
  T.union [
    "sequence">: gql "InterfaceTypeExtension_Sequence",
    "sequence2">: gql "InterfaceTypeExtension_Sequence2",
    "sequence3">: gql "InterfaceTypeExtension_Sequence3"]

interfaceTypeExtension_Sequence :: Binding
interfaceTypeExtension_Sequence = define "InterfaceTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.maybe $ gql "ImplementsInterfaces",
    "Directives">: T.maybe $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

interfaceTypeExtension_Sequence2 :: Binding
interfaceTypeExtension_Sequence2 = define "InterfaceTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.maybe $ gql "ImplementsInterfaces",
    "Directives">: gql "Directives"]

interfaceTypeExtension_Sequence3 :: Binding
interfaceTypeExtension_Sequence3 = define "InterfaceTypeExtension_Sequence3" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces"]

unionTypeDefinition :: Binding
unionTypeDefinition = define "UnionTypeDefinition" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.maybe $ gql "Directives",
    "UnionMemberTypes">: T.maybe $ gql "UnionMemberTypes"]

unionMemberTypes :: Binding
unionMemberTypes = define "UnionMemberTypes" $
  T.union [
    "sequence">: gql "UnionMemberTypes_Sequence",
    "sequence2">: gql "UnionMemberTypes_Sequence2"]

unionMemberTypes_Sequence :: Binding
unionMemberTypes_Sequence = define "UnionMemberTypes_Sequence" $
  T.record [
    "UnionMemberTypes">: gql "UnionMemberTypes",
    "NamedType">: gql "NamedType"]

unionMemberTypes_Sequence2 :: Binding
unionMemberTypes_Sequence2 = define "UnionMemberTypes_Sequence2" $
  T.record [
    "Or">: T.maybe T.unit,
    "NamedType">: gql "NamedType"]

unionTypeExtension :: Binding
unionTypeExtension = define "UnionTypeExtension" $
  T.union [
    "sequence">: gql "UnionTypeExtension_Sequence",
    "sequence2">: gql "UnionTypeExtension_Sequence2"]

unionTypeExtension_Sequence :: Binding
unionTypeExtension_Sequence = define "UnionTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.maybe $ gql "Directives",
    "UnionMemberTypes">: gql "UnionMemberTypes"]

unionTypeExtension_Sequence2 :: Binding
unionTypeExtension_Sequence2 = define "UnionTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

enumTypeDefinition :: Binding
enumTypeDefinition = define "EnumTypeDefinition" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.maybe $ gql "Directives",
    "EnumValuesDefinition">: T.maybe $ gql "EnumValuesDefinition"]

enumValuesDefinition :: Binding
enumValuesDefinition = define "EnumValuesDefinition" $ T.wrap $ T.list $ gql "EnumValueDefinition"

enumValueDefinition :: Binding
enumValueDefinition = define "EnumValueDefinition" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "EnumValue">: gql "EnumValue",
    "Directives">: T.maybe $ gql "Directives"]

enumTypeExtension :: Binding
enumTypeExtension = define "EnumTypeExtension" $
  T.union [
    "sequence">: gql "EnumTypeExtension_Sequence",
    "sequence2">: gql "EnumTypeExtension_Sequence2"]

enumTypeExtension_Sequence :: Binding
enumTypeExtension_Sequence = define "EnumTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.maybe $ gql "Directives",
    "EnumValuesDefinition">: gql "EnumValuesDefinition"]

enumTypeExtension_Sequence2 :: Binding
enumTypeExtension_Sequence2 = define "EnumTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

inputObjectTypeDefinition :: Binding
inputObjectTypeDefinition = define "InputObjectTypeDefinition" $
  T.union [
    "sequence">: gql "InputObjectTypeDefinition_Sequence",
    "sequence2">: gql "InputObjectTypeDefinition_Sequence2"]

inputObjectTypeDefinition_Sequence :: Binding
inputObjectTypeDefinition_Sequence = define "InputObjectTypeDefinition_Sequence" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.maybe $ gql "Directives",
    "InputFieldsDefinition">: gql "InputFieldsDefinition"]

inputObjectTypeDefinition_Sequence2 :: Binding
inputObjectTypeDefinition_Sequence2 = define "InputObjectTypeDefinition_Sequence2" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.maybe $ gql "Directives"]

inputFieldsDefinition :: Binding
inputFieldsDefinition = define "InputFieldsDefinition" $ T.wrap $ T.list $ gql "InputValueDefinition"

inputObjectTypeExtension :: Binding
inputObjectTypeExtension = define "InputObjectTypeExtension" $
  T.union [
    "sequence">: gql "InputObjectTypeExtension_Sequence",
    "sequence2">: gql "InputObjectTypeExtension_Sequence2"]

inputObjectTypeExtension_Sequence :: Binding
inputObjectTypeExtension_Sequence = define "InputObjectTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.maybe $ gql "Directives",
    "InputFieldsDefinition">: gql "InputFieldsDefinition"]

inputObjectTypeExtension_Sequence2 :: Binding
inputObjectTypeExtension_Sequence2 = define "InputObjectTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

-- Directive definitions

directiveDefinition :: Binding
directiveDefinition = define "DirectiveDefinition" $
  T.record [
    "Description">: T.maybe $ gql "Description",
    "Name">: gql "Name",
    "ArgumentsDefinition">: T.maybe $ gql "ArgumentsDefinition",
    "Repeatable">: T.maybe T.unit,
    "DirectiveLocations">: gql "DirectiveLocations"]

directiveLocations :: Binding
directiveLocations = define "DirectiveLocations" $
  T.union [
    "sequence">: gql "DirectiveLocations_Sequence",
    "sequence2">: gql "DirectiveLocations_Sequence2"]

directiveLocations_Sequence :: Binding
directiveLocations_Sequence = define "DirectiveLocations_Sequence" $
  T.record [
    "DirectiveLocations">: gql "DirectiveLocations",
    "DirectiveLocation">: gql "DirectiveLocation"]

directiveLocations_Sequence2 :: Binding
directiveLocations_Sequence2 = define "DirectiveLocations_Sequence2" $
  T.record [
    "Or">: T.maybe T.unit,
    "DirectiveLocation">: gql "DirectiveLocation"]

directiveLocation :: Binding
directiveLocation = define "DirectiveLocation" $
  T.union [
    "executable">: gql "ExecutableDirectiveLocation",
    "typeSystem">: gql "TypeSystemDirectiveLocation"]

executableDirectiveLocation :: Binding
executableDirectiveLocation = define "ExecutableDirectiveLocation" $
  T.union [
    "QUERY">: T.unit,
    "MUTATION">: T.unit,
    "SUBSCRIPTION">: T.unit,
    "FIELD">: T.unit,
    "FRAGMENT_DEFINITION">: T.unit,
    "FRAGMENT_SPREAD">: T.unit,
    "INLINE_FRAGMENT">: T.unit,
    "VARIABLE_DEFINITION">: T.unit]

typeSystemDirectiveLocation :: Binding
typeSystemDirectiveLocation = define "TypeSystemDirectiveLocation" $
  T.union [
    "SCHEMA">: T.unit,
    "SCALAR">: T.unit,
    "OBJECT">: T.unit,
    "FIELD_DEFINITION">: T.unit,
    "ARGUMENT_DEFINITION">: T.unit,
    "INTERFACE">: T.unit,
    "UNION">: T.unit,
    "ENUM">: T.unit,
    "ENUM_VALUE">: T.unit,
    "INPUT_OBJECT">: T.unit,
    "INPUT_FIELD_DEFINITION">: T.unit]
