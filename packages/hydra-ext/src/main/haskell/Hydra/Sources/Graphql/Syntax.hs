module Hydra.Sources.Graphql.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T


ns :: ModuleName
ns = ModuleName "hydra.graphql.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [],
            moduleMetadata = descriptionMetadata (Just ("A GraphQL model. Based on the (extended) BNF at:\n" ++
      "  https://spec.graphql.org/draft/#sec-Appendix-Grammar-Summary"))}
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

gql :: String -> Type
gql = typeref ns

-- Token definitions

floatValue :: TypeDefinition
floatValue = define "FloatValue" $ T.wrap T.string

intValue :: TypeDefinition
intValue = define "IntValue" $ T.wrap T.string

name_ :: TypeDefinition
name_ = define "Name" $ T.wrap T.string

stringValue :: TypeDefinition
stringValue = define "StringValue" $ T.wrap T.string

-- Document definitions

alias_ :: TypeDefinition
alias_ = define "Alias" $
  T.union [
    "Name">: gql "Name",
    "Colon">: T.unit]

argument :: TypeDefinition
argument = define "Argument" $
  T.record [
    "Name">: gql "Name",
    "Value">: gql "Value"]

arguments :: TypeDefinition
arguments = define "Arguments" $ T.wrap $ T.list $ gql "Argument"

booleanValue :: TypeDefinition
booleanValue = define "BooleanValue" $
  T.union [
    "True">: T.unit,
    "False">: T.unit]

defaultValue :: TypeDefinition
defaultValue = define "DefaultValue" $ T.wrap $ gql "Value"

definition :: TypeDefinition
definition = define "Definition" $
  T.union [
    "executable">: gql "ExecutableDefinition",
    "typeSystem">: gql "TypeSystemDefinitionOrExtension"]

directive :: TypeDefinition
directive = define "Directive" $
  T.record [
    "Name">: gql "Name",
    "Arguments">: T.optional $ gql "Arguments"]

directives :: TypeDefinition
directives = define "Directives" $ T.wrap $ T.list $ gql "Directive"

document :: TypeDefinition
document = define "Document" $ T.wrap $ T.list $ gql "Definition"

enumValue :: TypeDefinition
enumValue = define "EnumValue" $ T.wrap $ gql "Name"

executableDefinition :: TypeDefinition
executableDefinition = define "ExecutableDefinition" $
  T.union [
    "operation">: gql "OperationDefinition",
    "fragment">: gql "FragmentDefinition"]

executableDocument :: TypeDefinition
executableDocument = define "ExecutableDocument" $ T.wrap $ T.list $ gql "ExecutableDefinition"

field_ :: TypeDefinition
field_ = define "Field" $
  T.record [
    "Alias">: T.optional $ gql "Alias",
    "Name">: gql "Name",
    "Arguments">: T.optional $ gql "Arguments",
    "Directives">: T.optional $ gql "Directives",
    "SelectionSet">: T.optional $ gql "SelectionSet"]

fragmentDefinition :: TypeDefinition
fragmentDefinition = define "FragmentDefinition" $
  T.record [
    "FragmentName">: gql "FragmentName",
    "TypeCondition">: gql "TypeCondition",
    "Directives">: T.optional $ gql "Directives",
    "SelectionSet">: gql "SelectionSet"]

fragmentName :: TypeDefinition
fragmentName = define "FragmentName" $ T.wrap $ gql "Name"

fragmentSpread :: TypeDefinition
fragmentSpread = define "FragmentSpread" $
  T.record [
    "FragmentName">: gql "FragmentName",
    "Directives">: T.optional $ gql "Directives"]

inlineFragment :: TypeDefinition
inlineFragment = define "InlineFragment" $
  T.record [
    "TypeCondition">: T.optional $ gql "TypeCondition",
    "Directives">: T.optional $ gql "Directives",
    "SelectionSet">: gql "SelectionSet"]

listType :: TypeDefinition
listType = define "ListType" $ T.wrap $ gql "Type"

listValue :: TypeDefinition
listValue = define "ListValue" $
  T.union [
    "sequence">: gql "ListValue_Sequence",
    "sequence2">: T.list $ gql "Value"]

listValue_Sequence :: TypeDefinition
listValue_Sequence = define "ListValue_Sequence" $ T.record []

namedType :: TypeDefinition
namedType = define "NamedType" $ T.wrap $ gql "Name"

nonNullType :: TypeDefinition
nonNullType = define "NonNullType" $
  T.union [
    "named">: gql "NamedType",
    "list">: gql "ListType"]

nullValue :: TypeDefinition
nullValue = define "NullValue" $ T.wrap T.unit

objectField :: TypeDefinition
objectField = define "ObjectField" $
  T.record [
    "Name">: gql "Name",
    "Value">: gql "Value"]

objectValue :: TypeDefinition
objectValue = define "ObjectValue" $
  T.union [
    "sequence">: gql "ObjectValue_Sequence",
    "sequence2">: T.list $ gql "ObjectField"]

objectValue_Sequence :: TypeDefinition
objectValue_Sequence = define "ObjectValue_Sequence" $ T.record []

operationDefinition :: TypeDefinition
operationDefinition = define "OperationDefinition" $
  T.union [
    "sequence">: gql "OperationDefinition_Sequence",
    "SelectionSet">: gql "SelectionSet"]

operationDefinition_Sequence :: TypeDefinition
operationDefinition_Sequence = define "OperationDefinition_Sequence" $
  T.record [
    "OperationType">: gql "OperationType",
    "Name">: T.optional $ gql "Name",
    "VariablesDefinition">: T.optional $ gql "VariablesDefinition",
    "Directives">: T.optional $ gql "Directives",
    "SelectionSet">: gql "SelectionSet"]

operationType :: TypeDefinition
operationType = define "OperationType" $
  T.union [
    "Query">: T.unit,
    "Mutation">: T.unit,
    "Subscription">: T.unit]

selection :: TypeDefinition
selection = define "Selection" $
  T.union [
    "Field">: gql "Field",
    "FragmentSpread">: gql "FragmentSpread",
    "InlineFragment">: gql "InlineFragment"]

selectionSet :: TypeDefinition
selectionSet = define "SelectionSet" $ T.wrap $ T.list $ gql "Selection"

typeCondition :: TypeDefinition
typeCondition = define "TypeCondition" $
  T.union [
    "On">: T.unit,
    "NamedType">: gql "NamedType"]

type_ :: TypeDefinition
type_ = define "Type" $
  T.union [
    "named">: gql "NamedType",
    "list">: gql "ListType",
    "nonNull">: gql "NonNullType"]

value :: TypeDefinition
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

variable :: TypeDefinition
variable = define "Variable" $ T.wrap $ gql "Name"

variablesDefinition :: TypeDefinition
variablesDefinition = define "VariablesDefinition" $
  T.record [
    "Variable">: gql "Variable",
    "Type">: gql "Type",
    "DefaultValue">: T.optional $ gql "DefaultValue",
    "Directives">: T.optional $ gql "Directives"]

-- Type system definitions

argumentsDefinition :: TypeDefinition
argumentsDefinition = define "ArgumentsDefinition" $ T.wrap $ T.list $ gql "InputValueDefinition"

description :: TypeDefinition
description = define "Description" $ T.wrap $ gql "StringValue"

enumTypeDefinition :: TypeDefinition
enumTypeDefinition = define "EnumTypeDefinition" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "EnumValuesDefinition">: T.optional $ gql "EnumValuesDefinition"]

enumTypeExtension :: TypeDefinition
enumTypeExtension = define "EnumTypeExtension" $
  T.union [
    "sequence">: gql "EnumTypeExtension_Sequence",
    "sequence2">: gql "EnumTypeExtension_Sequence2"]

enumTypeExtension_Sequence :: TypeDefinition
enumTypeExtension_Sequence = define "EnumTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "EnumValuesDefinition">: gql "EnumValuesDefinition"]

enumTypeExtension_Sequence2 :: TypeDefinition
enumTypeExtension_Sequence2 = define "EnumTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

enumValueDefinition :: TypeDefinition
enumValueDefinition = define "EnumValueDefinition" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "EnumValue">: gql "EnumValue",
    "Directives">: T.optional $ gql "Directives"]

enumValuesDefinition :: TypeDefinition
enumValuesDefinition = define "EnumValuesDefinition" $ T.wrap $ T.list $ gql "EnumValueDefinition"

fieldDefinition :: TypeDefinition
fieldDefinition = define "FieldDefinition" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ArgumentsDefinition">: T.optional $ gql "ArgumentsDefinition",
    "Type">: gql "Type",
    "Directives">: T.optional $ gql "Directives"]

fieldsDefinition :: TypeDefinition
fieldsDefinition = define "FieldsDefinition" $ T.wrap $ T.list $ gql "FieldDefinition"

implementsInterfaces :: TypeDefinition
implementsInterfaces = define "ImplementsInterfaces" $
  T.union [
    "sequence">: gql "ImplementsInterfaces_Sequence",
    "sequence2">: gql "ImplementsInterfaces_Sequence2"]

implementsInterfaces_Sequence :: TypeDefinition
implementsInterfaces_Sequence = define "ImplementsInterfaces_Sequence" $
  T.record [
    "ImplementsInterfaces">: gql "ImplementsInterfaces",
    "NamedType">: gql "NamedType"]

implementsInterfaces_Sequence2 :: TypeDefinition
implementsInterfaces_Sequence2 = define "ImplementsInterfaces_Sequence2" $
  T.record [
    "Amp">: T.optional T.unit,
    "NamedType">: gql "NamedType"]

inputFieldsDefinition :: TypeDefinition
inputFieldsDefinition = define "InputFieldsDefinition" $ T.wrap $ T.list $ gql "InputValueDefinition"

inputObjectTypeDefinition :: TypeDefinition
inputObjectTypeDefinition = define "InputObjectTypeDefinition" $
  T.union [
    "sequence">: gql "InputObjectTypeDefinition_Sequence",
    "sequence2">: gql "InputObjectTypeDefinition_Sequence2"]

inputObjectTypeDefinition_Sequence :: TypeDefinition
inputObjectTypeDefinition_Sequence = define "InputObjectTypeDefinition_Sequence" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "InputFieldsDefinition">: gql "InputFieldsDefinition"]

inputObjectTypeDefinition_Sequence2 :: TypeDefinition
inputObjectTypeDefinition_Sequence2 = define "InputObjectTypeDefinition_Sequence2" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives"]

inputObjectTypeExtension :: TypeDefinition
inputObjectTypeExtension = define "InputObjectTypeExtension" $
  T.union [
    "sequence">: gql "InputObjectTypeExtension_Sequence",
    "sequence2">: gql "InputObjectTypeExtension_Sequence2"]

inputObjectTypeExtension_Sequence :: TypeDefinition
inputObjectTypeExtension_Sequence = define "InputObjectTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "InputFieldsDefinition">: gql "InputFieldsDefinition"]

inputObjectTypeExtension_Sequence2 :: TypeDefinition
inputObjectTypeExtension_Sequence2 = define "InputObjectTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

inputValueDefinition :: TypeDefinition
inputValueDefinition = define "InputValueDefinition" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Type">: gql "Type",
    "DefaultValue">: T.optional $ gql "DefaultValue",
    "Directives">: T.optional $ gql "Directives"]

interfaceTypeDefinition :: TypeDefinition
interfaceTypeDefinition = define "InterfaceTypeDefinition" $
  T.union [
    "sequence">: gql "InterfaceTypeDefinition_Sequence",
    "sequence2">: gql "InterfaceTypeDefinition_Sequence2"]

interfaceTypeDefinition_Sequence :: TypeDefinition
interfaceTypeDefinition_Sequence = define "InterfaceTypeDefinition_Sequence" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

interfaceTypeDefinition_Sequence2 :: TypeDefinition
interfaceTypeDefinition_Sequence2 = define "InterfaceTypeDefinition_Sequence2" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives"]

interfaceTypeExtension :: TypeDefinition
interfaceTypeExtension = define "InterfaceTypeExtension" $
  T.union [
    "sequence">: gql "InterfaceTypeExtension_Sequence",
    "sequence2">: gql "InterfaceTypeExtension_Sequence2",
    "sequence3">: gql "InterfaceTypeExtension_Sequence3"]

interfaceTypeExtension_Sequence :: TypeDefinition
interfaceTypeExtension_Sequence = define "InterfaceTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

interfaceTypeExtension_Sequence2 :: TypeDefinition
interfaceTypeExtension_Sequence2 = define "InterfaceTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: gql "Directives"]

interfaceTypeExtension_Sequence3 :: TypeDefinition
interfaceTypeExtension_Sequence3 = define "InterfaceTypeExtension_Sequence3" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces"]

objectTypeDefinition :: TypeDefinition
objectTypeDefinition = define "ObjectTypeDefinition" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: T.optional $ gql "FieldsDefinition"]

objectTypeExtension :: TypeDefinition
objectTypeExtension = define "ObjectTypeExtension" $
  T.union [
    "sequence">: gql "ObjectTypeExtension_Sequence",
    "sequence2">: gql "ObjectTypeExtension_Sequence2",
    "sequence3">: gql "ObjectTypeExtension_Sequence3"]

objectTypeExtension_Sequence :: TypeDefinition
objectTypeExtension_Sequence = define "ObjectTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

objectTypeExtension_Sequence2 :: TypeDefinition
objectTypeExtension_Sequence2 = define "ObjectTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives"]

objectTypeExtension_Sequence3 :: TypeDefinition
objectTypeExtension_Sequence3 = define "ObjectTypeExtension_Sequence3" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces"]

rootOperationTypeDefinition :: TypeDefinition
rootOperationTypeDefinition = define "RootOperationTypeDefinition" $
  T.record [
    "OperationType">: gql "OperationType",
    "NamedType">: gql "NamedType"]

scalarTypeDefinition :: TypeDefinition
scalarTypeDefinition = define "ScalarTypeDefinition" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives"]

scalarTypeExtension :: TypeDefinition
scalarTypeExtension = define "ScalarTypeExtension" $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

schemaDefinition :: TypeDefinition
schemaDefinition = define "SchemaDefinition" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Directives">: T.optional $ gql "Directives",
    "RootOperationTypeDefinition">: gql "RootOperationTypeDefinition"]

schemaExtension :: TypeDefinition
schemaExtension = define "SchemaExtension" $
  T.union [
    "sequence">: gql "SchemaExtension_Sequence",
    "sequence2">: gql "Directives"]

schemaExtension_Sequence :: TypeDefinition
schemaExtension_Sequence = define "SchemaExtension_Sequence" $
  T.record [
    "Directives">: T.optional $ gql "Directives",
    "RootOperationTypeDefinition">: gql "RootOperationTypeDefinition"]

typeDefinition :: TypeDefinition
typeDefinition = define "TypeDefinition" $
  T.union [
    "scalar">: gql "ScalarTypeDefinition",
    "object">: gql "ObjectTypeDefinition",
    "interface">: gql "InterfaceTypeDefinition",
    "union">: gql "UnionTypeDefinition",
    "enum">: gql "EnumTypeDefinition",
    "inputObject">: gql "InputObjectTypeDefinition"]

typeExtension :: TypeDefinition
typeExtension = define "TypeExtension" $
  T.union [
    "scalar">: gql "ScalarTypeExtension",
    "object">: gql "ObjectTypeExtension",
    "interface">: gql "InterfaceTypeExtension",
    "union">: gql "UnionTypeExtension",
    "enum">: gql "EnumTypeExtension",
    "inputObject">: gql "InputObjectTypeExtension"]

typeSystemDefinition :: TypeDefinition
typeSystemDefinition = define "TypeSystemDefinition" $
  T.union [
    "schema">: gql "SchemaDefinition",
    "type">: gql "TypeDefinition",
    "directive">: gql "DirectiveDefinition"]

typeSystemDefinitionOrExtension :: TypeDefinition
typeSystemDefinitionOrExtension = define "TypeSystemDefinitionOrExtension" $
  T.union [
    "definition">: gql "TypeSystemDefinition",
    "extension">: gql "TypeSystemExtension"]

typeSystemDocment :: TypeDefinition
typeSystemDocment = define "TypeSystemDocment" $ T.wrap $ T.list $ gql "TypeSystemDefinition"

typeSystemExtension :: TypeDefinition
typeSystemExtension = define "TypeSystemExtension" $
  T.union [
    "schema">: gql "SchemaExtension",
    "type">: gql "TypeExtension"]

typeSystemExtensionDocument :: TypeDefinition
typeSystemExtensionDocument = define "TypeSystemExtensionDocument" $
  T.wrap $ T.list $ gql "TypeSystemDefinitionOrExtension"

unionMemberTypes :: TypeDefinition
unionMemberTypes = define "UnionMemberTypes" $
  T.union [
    "sequence">: gql "UnionMemberTypes_Sequence",
    "sequence2">: gql "UnionMemberTypes_Sequence2"]

unionMemberTypes_Sequence :: TypeDefinition
unionMemberTypes_Sequence = define "UnionMemberTypes_Sequence" $
  T.record [
    "UnionMemberTypes">: gql "UnionMemberTypes",
    "NamedType">: gql "NamedType"]

unionMemberTypes_Sequence2 :: TypeDefinition
unionMemberTypes_Sequence2 = define "UnionMemberTypes_Sequence2" $
  T.record [
    "Or">: T.optional T.unit,
    "NamedType">: gql "NamedType"]

unionTypeDefinition :: TypeDefinition
unionTypeDefinition = define "UnionTypeDefinition" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "UnionMemberTypes">: T.optional $ gql "UnionMemberTypes"]

unionTypeExtension :: TypeDefinition
unionTypeExtension = define "UnionTypeExtension" $
  T.union [
    "sequence">: gql "UnionTypeExtension_Sequence",
    "sequence2">: gql "UnionTypeExtension_Sequence2"]

unionTypeExtension_Sequence :: TypeDefinition
unionTypeExtension_Sequence = define "UnionTypeExtension_Sequence" $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "UnionMemberTypes">: gql "UnionMemberTypes"]

unionTypeExtension_Sequence2 :: TypeDefinition
unionTypeExtension_Sequence2 = define "UnionTypeExtension_Sequence2" $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

-- Directive definitions

directiveDefinition :: TypeDefinition
directiveDefinition = define "DirectiveDefinition" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ArgumentsDefinition">: T.optional $ gql "ArgumentsDefinition",
    "Repeatable">: T.optional T.unit,
    "DirectiveLocations">: gql "DirectiveLocations"]

directiveLocation :: TypeDefinition
directiveLocation = define "DirectiveLocation" $
  T.union [
    "executable">: gql "ExecutableDirectiveLocation",
    "typeSystem">: gql "TypeSystemDirectiveLocation"]

directiveLocations :: TypeDefinition
directiveLocations = define "DirectiveLocations" $
  T.union [
    "sequence">: gql "DirectiveLocations_Sequence",
    "sequence2">: gql "DirectiveLocations_Sequence2"]

directiveLocations_Sequence :: TypeDefinition
directiveLocations_Sequence = define "DirectiveLocations_Sequence" $
  T.record [
    "DirectiveLocations">: gql "DirectiveLocations",
    "DirectiveLocation">: gql "DirectiveLocation"]

directiveLocations_Sequence2 :: TypeDefinition
directiveLocations_Sequence2 = define "DirectiveLocations_Sequence2" $
  T.record [
    "Or">: T.optional T.unit,
    "DirectiveLocation">: gql "DirectiveLocation"]

executableDirectiveLocation :: TypeDefinition
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

typeSystemDirectiveLocation :: TypeDefinition
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
