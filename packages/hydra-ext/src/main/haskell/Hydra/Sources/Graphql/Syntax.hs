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
      alias_,
      argument,
      arguments,
      argumentsDefinition,
      booleanValue,
      defaultValue,
      definition,
      description,
      directive,
      directiveDefinition,
      directiveLocation,
      directiveLocations,
      directiveLocationsSequence,
      directiveLocationsSequence2,
      directives,
      document,
      enumTypeDefinition,
      enumTypeExtension,
      enumTypeExtensionSequence,
      enumTypeExtensionSequence2,
      enumValue,
      enumValueDefinition,
      enumValuesDefinition,
      executableDefinition,
      executableDirectiveLocation,
      executableDocument,
      field_,
      fieldDefinition,
      fieldsDefinition,
      floatValue,
      fragmentDefinition,
      fragmentName,
      fragmentSpread,
      implementsInterfaces,
      implementsInterfacesSequence,
      implementsInterfacesSequence2,
      inlineFragment,
      inputFieldsDefinition,
      inputObjectTypeDefinition,
      inputObjectTypeDefinitionSequence,
      inputObjectTypeDefinitionSequence2,
      inputObjectTypeExtension,
      inputObjectTypeExtensionSequence,
      inputObjectTypeExtensionSequence2,
      inputValueDefinition,
      intValue,
      interfaceTypeDefinition,
      interfaceTypeDefinitionSequence,
      interfaceTypeDefinitionSequence2,
      interfaceTypeExtension,
      interfaceTypeExtensionSequence,
      interfaceTypeExtensionSequence2,
      interfaceTypeExtensionSequence3,
      listType,
      listValue,
      listValueSequence,
      name_,
      namedType,
      nonNullType,
      nullValue,
      objectField,
      objectTypeDefinition,
      objectTypeExtension,
      objectTypeExtensionSequence,
      objectTypeExtensionSequence2,
      objectTypeExtensionSequence3,
      objectValue,
      objectValueSequence,
      operationDefinition,
      operationDefinitionSequence,
      operationType,
      rootOperationTypeDefinition,
      scalarTypeDefinition,
      scalarTypeExtension,
      schemaDefinition,
      schemaExtension,
      schemaExtensionSequence,
      selection,
      selectionSet,
      stringValue,
      type_,
      typeCondition,
      typeDefinition,
      typeExtension,
      typeSystemDefinition,
      typeSystemDefinitionOrExtension,
      typeSystemDirectiveLocation,
      typeSystemDocment,
      typeSystemExtension,
      typeSystemExtensionDocument,
      unionMemberTypes,
      unionMemberTypesSequence,
      unionMemberTypesSequence2,
      unionTypeDefinition,
      unionTypeExtension,
      unionTypeExtensionSequence,
      unionTypeExtensionSequence2,
      value,
      variable,
      variablesDefinition]

gql :: String -> Type
gql = typeref ns

-- Token definitions

floatValue :: TypeDefinition
floatValue = define "FloatValue" $
  doc "A GraphQL floating-point literal." $ T.wrap T.string

intValue :: TypeDefinition
intValue = define "IntValue" $
  doc "A GraphQL integer literal." $ T.wrap T.string

name_ :: TypeDefinition
name_ = define "Name" $
  doc "A GraphQL name: an identifier matching [_A-Za-z][_0-9A-Za-z]*." $ T.wrap T.string

stringValue :: TypeDefinition
stringValue = define "StringValue" $
  doc "A GraphQL string literal." $ T.wrap T.string

-- Document definitions

alias_ :: TypeDefinition
alias_ = define "Alias" $
  doc "A GraphQL field alias, immediately followed by a colon." $
  T.union [
    "Name">: gql "Name",
    "Colon">: T.unit]

argument :: TypeDefinition
argument = define "Argument" $
  doc "A single GraphQL argument, consisting of a name and a value." $
  T.record [
    "Name">: gql "Name",
    "Value">: gql "Value"]

arguments :: TypeDefinition
arguments = define "Arguments" $
  doc "A parenthesized list of GraphQL arguments." $ T.wrap $ T.list $ gql "Argument"

booleanValue :: TypeDefinition
booleanValue = define "BooleanValue" $
  doc "A GraphQL boolean literal, either true or false." $
  T.union [
    "True">: T.unit,
    "False">: T.unit]

defaultValue :: TypeDefinition
defaultValue = define "DefaultValue" $
  doc "A default value for a GraphQL variable or input field, introduced by an equals sign." $ T.wrap $ gql "Value"

definition :: TypeDefinition
definition = define "Definition" $
  doc "A top-level GraphQL definition, either executable or part of the type system." $
  T.union [
    "executable">: gql "ExecutableDefinition",
    "typeSystem">: gql "TypeSystemDefinitionOrExtension"]

directive :: TypeDefinition
directive = define "Directive" $
  doc "A single GraphQL directive, consisting of a name and optional arguments." $
  T.record [
    "Name">: gql "Name",
    "Arguments">: T.optional $ gql "Arguments"]

directives :: TypeDefinition
directives = define "Directives" $
  doc "A list of GraphQL directives applied to a definition or selection." $ T.wrap $ T.list $ gql "Directive"

document :: TypeDefinition
document = define "Document" $
  doc "A GraphQL document, consisting of a list of definitions." $ T.wrap $ T.list $ gql "Definition"

enumValue :: TypeDefinition
enumValue = define "EnumValue" $
  doc "A GraphQL enum value, represented as a name (excluding true, false, and null)." $ T.wrap $ gql "Name"

executableDefinition :: TypeDefinition
executableDefinition = define "ExecutableDefinition" $
  doc "A GraphQL executable definition, either an operation or a fragment definition." $
  T.union [
    "operation">: gql "OperationDefinition",
    "fragment">: gql "FragmentDefinition"]

executableDocument :: TypeDefinition
executableDocument = define "ExecutableDocument" $
  doc "A GraphQL document consisting only of executable definitions." $ T.wrap $ T.list $ gql "ExecutableDefinition"

field_ :: TypeDefinition
field_ = define "Field" $
  doc "A GraphQL field selection, with optional alias, arguments, directives, and sub-selection set." $
  T.record [
    "Alias">: T.optional $ gql "Alias",
    "Name">: gql "Name",
    "Arguments">: T.optional $ gql "Arguments",
    "Directives">: T.optional $ gql "Directives",
    "SelectionSet">: T.optional $ gql "SelectionSet"]

fragmentDefinition :: TypeDefinition
fragmentDefinition = define "FragmentDefinition" $
  doc "A GraphQL fragment definition, naming a type condition and a selection set." $
  T.record [
    "FragmentName">: gql "FragmentName",
    "TypeCondition">: gql "TypeCondition",
    "Directives">: T.optional $ gql "Directives",
    "SelectionSet">: gql "SelectionSet"]

fragmentName :: TypeDefinition
fragmentName = define "FragmentName" $
  doc "The name of a GraphQL fragment (any name other than \"on\")." $ T.wrap $ gql "Name"

fragmentSpread :: TypeDefinition
fragmentSpread = define "FragmentSpread" $
  doc "A GraphQL fragment spread, referencing a named fragment with optional directives." $
  T.record [
    "FragmentName">: gql "FragmentName",
    "Directives">: T.optional $ gql "Directives"]

inlineFragment :: TypeDefinition
inlineFragment = define "InlineFragment" $
  doc "A GraphQL inline fragment, with optional type condition, directives, and a selection set." $
  T.record [
    "TypeCondition">: T.optional $ gql "TypeCondition",
    "Directives">: T.optional $ gql "Directives",
    "SelectionSet">: gql "SelectionSet"]

listType :: TypeDefinition
listType = define "ListType" $
  doc "A GraphQL list type, wrapping an inner type in square brackets." $ T.wrap $ gql "Type"

listValue :: TypeDefinition
listValue = define "ListValue" $
  doc "A GraphQL list literal, either empty or containing a sequence of values." $
  T.union [
    "seq">: gql "ListValueSequence",
    "sequence2">: T.list $ gql "Value"]

listValueSequence :: TypeDefinition
listValueSequence = define "ListValueSequence" $
  doc "The empty-list form of a GraphQL ListValue." T.unit

namedType :: TypeDefinition
namedType = define "NamedType" $
  doc "A GraphQL named type, referencing a type by its name." $ T.wrap $ gql "Name"

nonNullType :: TypeDefinition
nonNullType = define "NonNullType" $
  doc "A GraphQL non-null type, wrapping a named or list type with a trailing exclamation mark." $
  T.union [
    "named">: gql "NamedType",
    "list">: gql "ListType"]

nullValue :: TypeDefinition
nullValue = define "NullValue" $
  doc "The GraphQL null literal." $ T.wrap T.unit

objectField :: TypeDefinition
objectField = define "ObjectField" $
  doc "A single field of a GraphQL object literal, consisting of a name and a value." $
  T.record [
    "Name">: gql "Name",
    "Value">: gql "Value"]

objectValue :: TypeDefinition
objectValue = define "ObjectValue" $
  doc "A GraphQL object literal, either empty or containing a sequence of object fields." $
  T.union [
    "seq">: gql "ObjectValueSequence",
    "sequence2">: T.list $ gql "ObjectField"]

objectValueSequence :: TypeDefinition
objectValueSequence = define "ObjectValueSequence" $
  doc "The empty-object form of a GraphQL ObjectValue." T.unit

operationDefinition :: TypeDefinition
operationDefinition = define "OperationDefinition" $
  doc "A GraphQL operation definition, either a shorthand query or an explicit operation." $
  T.union [
    "seq">: gql "OperationDefinitionSequence",
    "SelectionSet">: gql "SelectionSet"]

operationDefinitionSequence :: TypeDefinition
operationDefinitionSequence = define "OperationDefinitionSequence" $
  doc "An explicit GraphQL operation definition with a type, optional name, variables, and directives." $
  T.record [
    "OperationType">: gql "OperationType",
    "Name">: T.optional $ gql "Name",
    "VariablesDefinition">: T.optional $ gql "VariablesDefinition",
    "Directives">: T.optional $ gql "Directives",
    "SelectionSet">: gql "SelectionSet"]

operationType :: TypeDefinition
operationType = define "OperationType" $
  doc "The type of a GraphQL operation: query, mutation, or subscription." $
  T.union [
    "Query">: T.unit,
    "Mutation">: T.unit,
    "Subscription">: T.unit]

selection :: TypeDefinition
selection = define "Selection" $
  doc "A single selection within a GraphQL selection set: a field, fragment spread, or inline fragment." $
  T.union [
    "Field">: gql "Field",
    "FragmentSpread">: gql "FragmentSpread",
    "InlineFragment">: gql "InlineFragment"]

selectionSet :: TypeDefinition
selectionSet = define "SelectionSet" $
  doc "A braced list of GraphQL selections." $ T.wrap $ T.list $ gql "Selection"

typeCondition :: TypeDefinition
typeCondition = define "TypeCondition" $
  doc "A GraphQL type condition, naming the type a fragment applies to." $
  T.union [
    "On">: T.unit,
    "NamedType">: gql "NamedType"]

type_ :: TypeDefinition
type_ = define "Type" $
  doc "A GraphQL type reference: named, list, or non-null." $
  T.union [
    "named">: gql "NamedType",
    "list">: gql "ListType",
    "nonNull">: gql "NonNullType"]

value :: TypeDefinition
value = define "Value" $
  doc "A GraphQL value: a variable, literal, list, or object." $
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
variable = define "Variable" $
  doc "A GraphQL variable reference, introduced by a dollar sign." $ T.wrap $ gql "Name"

variablesDefinition :: TypeDefinition
variablesDefinition = define "VariablesDefinition" $
  doc "A single variable definition in a GraphQL operation's variable list." $
  T.record [
    "Variable">: gql "Variable",
    "Type">: gql "Type",
    "DefaultValue">: T.optional $ gql "DefaultValue",
    "Directives">: T.optional $ gql "Directives"]

-- Type system definitions

argumentsDefinition :: TypeDefinition
argumentsDefinition = define "ArgumentsDefinition" $
  doc "A parenthesized list of GraphQL input value definitions for a field's arguments." $ T.wrap $ T.list $ gql "InputValueDefinition"

description :: TypeDefinition
description = define "Description" $
  doc "An optional string description preceding a GraphQL type system definition." $ T.wrap $ gql "StringValue"

enumTypeDefinition :: TypeDefinition
enumTypeDefinition = define "EnumTypeDefinition" $
  doc "A GraphQL enum type definition, giving its name, directives, and enum values." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "EnumValuesDefinition">: T.optional $ gql "EnumValuesDefinition"]

enumTypeExtension :: TypeDefinition
enumTypeExtension = define "EnumTypeExtension" $
  doc "An extension of a GraphQL enum type, either adding enum values or only directives." $
  T.union [
    "seq">: gql "EnumTypeExtensionSequence",
    "seq2">: gql "EnumTypeExtensionSequence2"]

enumTypeExtensionSequence :: TypeDefinition
enumTypeExtensionSequence = define "EnumTypeExtensionSequence" $
  doc "An enum type extension that adds new enum values, with optional directives." $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "EnumValuesDefinition">: gql "EnumValuesDefinition"]

enumTypeExtensionSequence2 :: TypeDefinition
enumTypeExtensionSequence2 = define "EnumTypeExtensionSequence2" $
  doc "An enum type extension consisting of directives only, with no new enum values." $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

enumValueDefinition :: TypeDefinition
enumValueDefinition = define "EnumValueDefinition" $
  doc "A single value definition within a GraphQL enum type, with optional description and directives." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "EnumValue">: gql "EnumValue",
    "Directives">: T.optional $ gql "Directives"]

enumValuesDefinition :: TypeDefinition
enumValuesDefinition = define "EnumValuesDefinition" $
  doc "The braced list of enum value definitions in a GraphQL enum type." $ T.wrap $ T.list $ gql "EnumValueDefinition"

fieldDefinition :: TypeDefinition
fieldDefinition = define "FieldDefinition" $
  doc "A field definition within a GraphQL object or interface type." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ArgumentsDefinition">: T.optional $ gql "ArgumentsDefinition",
    "Type">: gql "Type",
    "Directives">: T.optional $ gql "Directives"]

fieldsDefinition :: TypeDefinition
fieldsDefinition = define "FieldsDefinition" $
  doc "The braced list of field definitions in a GraphQL object or interface type." $ T.wrap $ T.list $ gql "FieldDefinition"

implementsInterfaces :: TypeDefinition
implementsInterfaces = define "ImplementsInterfaces" $
  doc "A clause declaring the interfaces implemented by a GraphQL object or interface type." $
  T.union [
    "seq">: gql "ImplementsInterfacesSequence",
    "seq2">: gql "ImplementsInterfacesSequence2"]

implementsInterfacesSequence :: TypeDefinition
implementsInterfacesSequence = define "ImplementsInterfacesSequence" $
  doc "A non-empty ImplementsInterfaces list with a leading list of interfaces." $
  T.record [
    "ImplementsInterfaces">: gql "ImplementsInterfaces",
    "NamedType">: gql "NamedType"]

implementsInterfacesSequence2 :: TypeDefinition
implementsInterfacesSequence2 = define "ImplementsInterfacesSequence2" $
  doc "The base case of an ImplementsInterfaces list: a single, optionally ampersand-prefixed interface." $
  T.record [
    "Amp">: T.optional T.unit,
    "NamedType">: gql "NamedType"]

inputFieldsDefinition :: TypeDefinition
inputFieldsDefinition = define "InputFieldsDefinition" $
  doc "The braced list of input field definitions in a GraphQL input object type." $ T.wrap $ T.list $ gql "InputValueDefinition"

inputObjectTypeDefinition :: TypeDefinition
inputObjectTypeDefinition = define "InputObjectTypeDefinition" $
  doc "A GraphQL input object type definition, either with or without input fields." $
  T.union [
    "seq">: gql "InputObjectTypeDefinitionSequence",
    "seq2">: gql "InputObjectTypeDefinitionSequence2"]

inputObjectTypeDefinitionSequence :: TypeDefinition
inputObjectTypeDefinitionSequence = define "InputObjectTypeDefinitionSequence" $
  doc "An input object type definition that includes an input fields definition." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "InputFieldsDefinition">: gql "InputFieldsDefinition"]

inputObjectTypeDefinitionSequence2 :: TypeDefinition
inputObjectTypeDefinitionSequence2 = define "InputObjectTypeDefinitionSequence2" $
  doc "An input object type definition with no input fields definition." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives"]

inputObjectTypeExtension :: TypeDefinition
inputObjectTypeExtension = define "InputObjectTypeExtension" $
  doc "An extension of a GraphQL input object type, either with or without input fields." $
  T.union [
    "seq">: gql "InputObjectTypeExtensionSequence",
    "seq2">: gql "InputObjectTypeExtensionSequence2"]

inputObjectTypeExtensionSequence :: TypeDefinition
inputObjectTypeExtensionSequence = define "InputObjectTypeExtensionSequence" $
  doc "An input object type extension that adds an input fields definition." $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "InputFieldsDefinition">: gql "InputFieldsDefinition"]

inputObjectTypeExtensionSequence2 :: TypeDefinition
inputObjectTypeExtensionSequence2 = define "InputObjectTypeExtensionSequence2" $
  doc "An input object type extension consisting of directives only." $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

inputValueDefinition :: TypeDefinition
inputValueDefinition = define "InputValueDefinition" $
  doc "A single input value definition, used for field arguments and input object fields." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Type">: gql "Type",
    "DefaultValue">: T.optional $ gql "DefaultValue",
    "Directives">: T.optional $ gql "Directives"]

interfaceTypeDefinition :: TypeDefinition
interfaceTypeDefinition = define "InterfaceTypeDefinition" $
  doc "A GraphQL interface type definition, either with or without a fields definition." $
  T.union [
    "seq">: gql "InterfaceTypeDefinitionSequence",
    "seq2">: gql "InterfaceTypeDefinitionSequence2"]

interfaceTypeDefinitionSequence :: TypeDefinition
interfaceTypeDefinitionSequence = define "InterfaceTypeDefinitionSequence" $
  doc "An interface type definition that includes a fields definition." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

interfaceTypeDefinitionSequence2 :: TypeDefinition
interfaceTypeDefinitionSequence2 = define "InterfaceTypeDefinitionSequence2" $
  doc "An interface type definition with no fields definition." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives"]

interfaceTypeExtension :: TypeDefinition
interfaceTypeExtension = define "InterfaceTypeExtension" $
  doc "An extension of a GraphQL interface type, in one of three forms." $
  T.union [
    "seq">: gql "InterfaceTypeExtensionSequence",
    "seq2">: gql "InterfaceTypeExtensionSequence2",
    "seq3">: gql "InterfaceTypeExtensionSequence3"]

interfaceTypeExtensionSequence :: TypeDefinition
interfaceTypeExtensionSequence = define "InterfaceTypeExtensionSequence" $
  doc "An interface type extension that adds a fields definition." $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

interfaceTypeExtensionSequence2 :: TypeDefinition
interfaceTypeExtensionSequence2 = define "InterfaceTypeExtensionSequence2" $
  doc "An interface type extension that adds directives but no fields definition." $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: gql "Directives"]

interfaceTypeExtensionSequence3 :: TypeDefinition
interfaceTypeExtensionSequence3 = define "InterfaceTypeExtensionSequence3" $
  doc "An interface type extension that adds only implemented interfaces." $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces"]

objectTypeDefinition :: TypeDefinition
objectTypeDefinition = define "ObjectTypeDefinition" $
  doc "A GraphQL object type definition, giving its name, interfaces, directives, and fields." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: T.optional $ gql "FieldsDefinition"]

objectTypeExtension :: TypeDefinition
objectTypeExtension = define "ObjectTypeExtension" $
  doc "An extension of a GraphQL object type, in one of three forms." $
  T.union [
    "seq">: gql "ObjectTypeExtensionSequence",
    "seq2">: gql "ObjectTypeExtensionSequence2",
    "seq3">: gql "ObjectTypeExtensionSequence3"]

objectTypeExtensionSequence :: TypeDefinition
objectTypeExtensionSequence = define "ObjectTypeExtensionSequence" $
  doc "An object type extension that adds a fields definition." $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives",
    "FieldsDefinition">: gql "FieldsDefinition"]

objectTypeExtensionSequence2 :: TypeDefinition
objectTypeExtensionSequence2 = define "ObjectTypeExtensionSequence2" $
  doc "An object type extension that adds directives but no fields definition." $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: T.optional $ gql "ImplementsInterfaces",
    "Directives">: T.optional $ gql "Directives"]

objectTypeExtensionSequence3 :: TypeDefinition
objectTypeExtensionSequence3 = define "ObjectTypeExtensionSequence3" $
  doc "An object type extension that adds only implemented interfaces." $
  T.record [
    "Name">: gql "Name",
    "ImplementsInterfaces">: gql "ImplementsInterfaces"]

rootOperationTypeDefinition :: TypeDefinition
rootOperationTypeDefinition = define "RootOperationTypeDefinition" $
  doc "A mapping from a GraphQL operation type to its root object type." $
  T.record [
    "OperationType">: gql "OperationType",
    "NamedType">: gql "NamedType"]

scalarTypeDefinition :: TypeDefinition
scalarTypeDefinition = define "ScalarTypeDefinition" $
  doc "A GraphQL scalar type definition, with optional description and directives." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives"]

scalarTypeExtension :: TypeDefinition
scalarTypeExtension = define "ScalarTypeExtension" $
  doc "An extension of a GraphQL scalar type, adding directives." $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

schemaDefinition :: TypeDefinition
schemaDefinition = define "SchemaDefinition" $
  doc "A GraphQL schema definition, declaring its root operation types." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Directives">: T.optional $ gql "Directives",
    "RootOperationTypeDefinition">: gql "RootOperationTypeDefinition"]

schemaExtension :: TypeDefinition
schemaExtension = define "SchemaExtension" $
  doc "An extension of a GraphQL schema, adding root operation types or directives." $
  T.union [
    "seq">: gql "SchemaExtensionSequence",
    "sequence2">: gql "Directives"]

schemaExtensionSequence :: TypeDefinition
schemaExtensionSequence = define "SchemaExtensionSequence" $
  doc "A schema extension that adds a root operation type definition." $
  T.record [
    "Directives">: T.optional $ gql "Directives",
    "RootOperationTypeDefinition">: gql "RootOperationTypeDefinition"]

typeDefinition :: TypeDefinition
typeDefinition = define "TypeDefinition" $
  doc "A GraphQL type system definition for a single named type." $
  T.union [
    "scalar">: gql "ScalarTypeDefinition",
    "object">: gql "ObjectTypeDefinition",
    "interface">: gql "InterfaceTypeDefinition",
    "union">: gql "UnionTypeDefinition",
    "enum">: gql "EnumTypeDefinition",
    "inputObject">: gql "InputObjectTypeDefinition"]

typeExtension :: TypeDefinition
typeExtension = define "TypeExtension" $
  doc "An extension of a GraphQL named type." $
  T.union [
    "scalar">: gql "ScalarTypeExtension",
    "object">: gql "ObjectTypeExtension",
    "interface">: gql "InterfaceTypeExtension",
    "union">: gql "UnionTypeExtension",
    "enum">: gql "EnumTypeExtension",
    "inputObject">: gql "InputObjectTypeExtension"]

typeSystemDefinition :: TypeDefinition
typeSystemDefinition = define "TypeSystemDefinition" $
  doc "A GraphQL type system definition: schema, type, or directive." $
  T.union [
    "schema">: gql "SchemaDefinition",
    "type">: gql "TypeDefinition",
    "directive">: gql "DirectiveDefinition"]

typeSystemDefinitionOrExtension :: TypeDefinition
typeSystemDefinitionOrExtension = define "TypeSystemDefinitionOrExtension" $
  doc "Either a GraphQL type system definition or an extension of one." $
  T.union [
    "definition">: gql "TypeSystemDefinition",
    "extension">: gql "TypeSystemExtension"]

typeSystemDocment :: TypeDefinition
typeSystemDocment = define "TypeSystemDocment" $
  doc "A GraphQL document consisting only of type system definitions." $ T.wrap $ T.list $ gql "TypeSystemDefinition"

typeSystemExtension :: TypeDefinition
typeSystemExtension = define "TypeSystemExtension" $
  doc "An extension of a GraphQL schema or type." $
  T.union [
    "schema">: gql "SchemaExtension",
    "type">: gql "TypeExtension"]

typeSystemExtensionDocument :: TypeDefinition
typeSystemExtensionDocument = define "TypeSystemExtensionDocument" $
  doc "A GraphQL document consisting of type system definitions and extensions." $
  T.wrap $ T.list $ gql "TypeSystemDefinitionOrExtension"

unionMemberTypes :: TypeDefinition
unionMemberTypes = define "UnionMemberTypes" $
  doc "The list of member types of a GraphQL union type, introduced by an equals sign." $
  T.union [
    "seq">: gql "UnionMemberTypesSequence",
    "seq2">: gql "UnionMemberTypesSequence2"]

unionMemberTypesSequence :: TypeDefinition
unionMemberTypesSequence = define "UnionMemberTypesSequence" $
  doc "A non-empty UnionMemberTypes list with a leading list of member types." $
  T.record [
    "UnionMemberTypes">: gql "UnionMemberTypes",
    "NamedType">: gql "NamedType"]

unionMemberTypesSequence2 :: TypeDefinition
unionMemberTypesSequence2 = define "UnionMemberTypesSequence2" $
  doc "The base case of a UnionMemberTypes list: a single, optionally pipe-prefixed member type." $
  T.record [
    "Or">: T.optional T.unit,
    "NamedType">: gql "NamedType"]

unionTypeDefinition :: TypeDefinition
unionTypeDefinition = define "UnionTypeDefinition" $
  doc "A GraphQL union type definition, giving its name, directives, and member types." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "UnionMemberTypes">: T.optional $ gql "UnionMemberTypes"]

unionTypeExtension :: TypeDefinition
unionTypeExtension = define "UnionTypeExtension" $
  doc "An extension of a GraphQL union type, either adding member types or only directives." $
  T.union [
    "seq">: gql "UnionTypeExtensionSequence",
    "seq2">: gql "UnionTypeExtensionSequence2"]

unionTypeExtensionSequence :: TypeDefinition
unionTypeExtensionSequence = define "UnionTypeExtensionSequence" $
  doc "A union type extension that adds new member types." $
  T.record [
    "Name">: gql "Name",
    "Directives">: T.optional $ gql "Directives",
    "UnionMemberTypes">: gql "UnionMemberTypes"]

unionTypeExtensionSequence2 :: TypeDefinition
unionTypeExtensionSequence2 = define "UnionTypeExtensionSequence2" $
  doc "A union type extension consisting of directives only." $
  T.record [
    "Name">: gql "Name",
    "Directives">: gql "Directives"]

-- Directive definitions

directiveDefinition :: TypeDefinition
directiveDefinition = define "DirectiveDefinition" $
  doc "A GraphQL directive definition, giving its name, arguments, repeatability, and locations." $
  T.record [
    "Description">: T.optional $ gql "Description",
    "Name">: gql "Name",
    "ArgumentsDefinition">: T.optional $ gql "ArgumentsDefinition",
    "Repeatable">: T.optional T.unit,
    "DirectiveLocations">: gql "DirectiveLocations"]

directiveLocation :: TypeDefinition
directiveLocation = define "DirectiveLocation" $
  doc "A location at which a GraphQL directive may be applied, either executable or type-system." $
  T.union [
    "executable">: gql "ExecutableDirectiveLocation",
    "typeSystem">: gql "TypeSystemDirectiveLocation"]

directiveLocations :: TypeDefinition
directiveLocations = define "DirectiveLocations" $
  doc "A pipe-separated list of GraphQL directive locations." $
  T.union [
    "seq">: gql "DirectiveLocationsSequence",
    "seq2">: gql "DirectiveLocationsSequence2"]

directiveLocationsSequence :: TypeDefinition
directiveLocationsSequence = define "DirectiveLocationsSequence" $
  doc "A non-empty DirectiveLocations sequence with a leading list of locations." $
  T.record [
    "DirectiveLocations">: gql "DirectiveLocations",
    "DirectiveLocation">: gql "DirectiveLocation"]

directiveLocationsSequence2 :: TypeDefinition
directiveLocationsSequence2 = define "DirectiveLocationsSequence2" $
  doc "The base case of a DirectiveLocations sequence: a single, optionally pipe-prefixed location." $
  T.record [
    "Or">: T.optional T.unit,
    "DirectiveLocation">: gql "DirectiveLocation"]

executableDirectiveLocation :: TypeDefinition
executableDirectiveLocation = define "ExecutableDirectiveLocation" $
  doc "A directive location valid on GraphQL executable definitions (queries, fields, fragments, etc.)." $
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
  doc "A directive location valid on GraphQL type system definitions." $
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
