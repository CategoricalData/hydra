module Hydra.Sources.Graphql.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T


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
      operationDefinitionSequence,
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
      listValueSequence,
      objectValue,
      objectValueSequence,
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
      schemaExtensionSequence,
      rootOperationTypeDefinition,
      description,
      typeDefinition,
      typeExtension,
      scalarTypeDefinition,
      scalarTypeExtension,
      objectTypeDefinition,
      objectTypeExtension,
      objectTypeExtensionSequence,
      objectTypeExtensionSequence2,
      objectTypeExtensionSequence3,
      implementsInterfaces,
      implementsInterfacesSequence,
      implementsInterfacesSequence2,
      fieldsDefinition,
      fieldDefinition,
      argumentsDefinition,
      inputValueDefinition,
      interfaceTypeDefinition,
      interfaceTypeDefinitionSequence,
      interfaceTypeDefinitionSequence2,
      interfaceTypeExtension,
      interfaceTypeExtensionSequence,
      interfaceTypeExtensionSequence2,
      interfaceTypeExtensionSequence3,
      unionTypeDefinition,
      unionMemberTypes,
      unionMemberTypesSequence,
      unionMemberTypesSequence2,
      unionTypeExtension,
      unionTypeExtensionSequence,
      unionTypeExtensionSequence2,
      enumTypeDefinition,
      enumValuesDefinition,
      enumValueDefinition,
      enumTypeExtension,
      enumTypeExtensionSequence,
      enumTypeExtensionSequence2,
      inputObjectTypeDefinition,
      inputObjectTypeDefinitionSequence,
      inputObjectTypeDefinitionSequence2,
      inputFieldsDefinition,
      inputObjectTypeExtension,
      inputObjectTypeExtensionSequence,
      inputObjectTypeExtensionSequence2,
      directiveDefinition,
      directiveLocations,
      directiveLocationsSequence,
      directiveLocationsSequence2,
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
    "seq">: gql "ListValueSequence",
    "sequence2">: T.list $ gql "Value"]

listValueSequence :: TypeDefinition
listValueSequence = define "ListValueSequence" T.unit

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
    "seq">: gql "ObjectValueSequence",
    "sequence2">: T.list $ gql "ObjectField"]

objectValueSequence :: TypeDefinition
objectValueSequence = define "ObjectValueSequence" T.unit

operationDefinition :: TypeDefinition
operationDefinition = define "OperationDefinition" $
  T.union [
    "seq">: gql "OperationDefinitionSequence",
    "SelectionSet">: gql "SelectionSet"]

operationDefinitionSequence :: TypeDefinition
operationDefinitionSequence = define "OperationDefinitionSequence" $
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
    "seq">: gql "EnumTypeExtensionSequence",
    "seq2">: gql "EnumTypeExtensionSequence2"]

enumTypeExtensionSequence :: TypeDefinition
enumTypeExtensionSequence = define "EnumTypeExtensionSequence" $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "EnumValuesDefinition">: gql "EnumValuesDefinition"]

enumTypeExtensionSequence2 :: TypeDefinition
enumTypeExtensionSequence2 = define "EnumTypeExtensionSequence2" $
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
    "seq">: gql "ImplementsInterfacesSequence",
    "seq2">: gql "ImplementsInterfacesSequence2"]

implementsInterfacesSequence :: TypeDefinition
implementsInterfacesSequence = define "ImplementsInterfacesSequence" $
  T.record [
    "ImplementsInterfaces">: gql "ImplementsInterfaces",
    "NamedType">: gql "NamedType"]

implementsInterfacesSequence2 :: TypeDefinition
implementsInterfacesSequence2 = define "ImplementsInterfacesSequence2" $
  T.record [
    "Amp">: T.optional T.unit,
    "NamedType">: gql "NamedType"]

inputFieldsDefinition :: TypeDefinition
inputFieldsDefinition = define "InputFieldsDefinition" $ T.wrap $ T.list $ gql "InputValueDefinition"

inputObjectTypeDefinition :: TypeDefinition
inputObjectTypeDefinition = define "InputObjectTypeDefinition" $
  T.union [
    "seq">: gql "InputObjectTypeDefinitionSequence",
    "seq2">: gql "InputObjectTypeDefinitionSequence2"]

inputObjectTypeDefinitionSequence :: TypeDefinition
inputObjectTypeDefinitionSequence = define "InputObjectTypeDefinitionSequence" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "InputFieldsDefinition">: gql "InputFieldsDefinition"]

inputObjectTypeDefinitionSequence2 :: TypeDefinition
inputObjectTypeDefinitionSequence2 = define "InputObjectTypeDefinitionSequence2" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives"]

inputObjectTypeExtension :: TypeDefinition
inputObjectTypeExtension = define "InputObjectTypeExtension" $
  T.union [
    "seq">: gql "InputObjectTypeExtensionSequence",
    "seq2">: gql "InputObjectTypeExtensionSequence2"]

inputObjectTypeExtensionSequence :: TypeDefinition
inputObjectTypeExtensionSequence = define "InputObjectTypeExtensionSequence" $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "InputFieldsDefinition">: gql "InputFieldsDefinition"]

inputObjectTypeExtensionSequence2 :: TypeDefinition
inputObjectTypeExtensionSequence2 = define "InputObjectTypeExtensionSequence2" $
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
    "seq">: gql "InterfaceTypeDefinitionSequence",
    "seq2">: gql "InterfaceTypeDefinitionSequence2"]

interfaceTypeDefinitionSequence :: TypeDefinition
interfaceTypeDefinitionSequence = define "InterfaceTypeDefinitionSequence" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

interfaceTypeDefinitionSequence2 :: TypeDefinition
interfaceTypeDefinitionSequence2 = define "InterfaceTypeDefinitionSequence2" $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives"]

interfaceTypeExtension :: TypeDefinition
interfaceTypeExtension = define "InterfaceTypeExtension" $
  T.union [
    "seq">: gql "InterfaceTypeExtensionSequence",
    "seq2">: gql "InterfaceTypeExtensionSequence2",
    "seq3">: gql "InterfaceTypeExtensionSequence3"]

interfaceTypeExtensionSequence :: TypeDefinition
interfaceTypeExtensionSequence = define "InterfaceTypeExtensionSequence" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

interfaceTypeExtensionSequence2 :: TypeDefinition
interfaceTypeExtensionSequence2 = define "InterfaceTypeExtensionSequence2" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: gql "Directives"]

interfaceTypeExtensionSequence3 :: TypeDefinition
interfaceTypeExtensionSequence3 = define "InterfaceTypeExtensionSequence3" $
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
    "seq">: gql "ObjectTypeExtensionSequence",
    "seq2">: gql "ObjectTypeExtensionSequence2",
    "seq3">: gql "ObjectTypeExtensionSequence3"]

objectTypeExtensionSequence :: TypeDefinition
objectTypeExtensionSequence = define "ObjectTypeExtensionSequence" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

objectTypeExtensionSequence2 :: TypeDefinition
objectTypeExtensionSequence2 = define "ObjectTypeExtensionSequence2" $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives"]

objectTypeExtensionSequence3 :: TypeDefinition
objectTypeExtensionSequence3 = define "ObjectTypeExtensionSequence3" $
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
    "seq">: gql "SchemaExtensionSequence",
    "sequence2">: gql "Directives"]

schemaExtensionSequence :: TypeDefinition
schemaExtensionSequence = define "SchemaExtensionSequence" $
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
    "seq">: gql "UnionMemberTypesSequence",
    "seq2">: gql "UnionMemberTypesSequence2"]

unionMemberTypesSequence :: TypeDefinition
unionMemberTypesSequence = define "UnionMemberTypesSequence" $
  T.record [
    "UnionMemberTypes">: gql "UnionMemberTypes",
    "NamedType">: gql "NamedType"]

unionMemberTypesSequence2 :: TypeDefinition
unionMemberTypesSequence2 = define "UnionMemberTypesSequence2" $
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
    "seq">: gql "UnionTypeExtensionSequence",
    "seq2">: gql "UnionTypeExtensionSequence2"]

unionTypeExtensionSequence :: TypeDefinition
unionTypeExtensionSequence = define "UnionTypeExtensionSequence" $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "UnionMemberTypes">: gql "UnionMemberTypes"]

unionTypeExtensionSequence2 :: TypeDefinition
unionTypeExtensionSequence2 = define "UnionTypeExtensionSequence2" $
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
    "seq">: gql "DirectiveLocationsSequence",
    "seq2">: gql "DirectiveLocationsSequence2"]

directiveLocationsSequence :: TypeDefinition
directiveLocationsSequence = define "DirectiveLocationsSequence" $
  T.record [
    "DirectiveLocations">: gql "DirectiveLocations",
    "DirectiveLocation">: gql "DirectiveLocation"]

directiveLocationsSequence2 :: TypeDefinition
directiveLocationsSequence2 = define "DirectiveLocationsSequence2" $
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
