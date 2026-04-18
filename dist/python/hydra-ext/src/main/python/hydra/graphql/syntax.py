# Note: this is an automatically generated file. Do not edit.

r"""A GraphQL model. Based on the (extended) BNF at:
  https://spec.graphql.org/draft/#sec-Appendix-Grammar-Summary."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class Name(Node[str]):
    ...

Name.TYPE_ = hydra.core.Name("hydra.graphql.syntax.Name")

class IntValue(Node[str]):
    ...

IntValue.TYPE_ = hydra.core.Name("hydra.graphql.syntax.IntValue")

class FloatValue(Node[str]):
    ...

FloatValue.TYPE_ = hydra.core.Name("hydra.graphql.syntax.FloatValue")

class StringValue(Node[str]):
    ...

StringValue.TYPE_ = hydra.core.Name("hydra.graphql.syntax.StringValue")

class Document(Node["frozenlist[Definition]"]):
    ...

Document.TYPE_ = hydra.core.Name("hydra.graphql.syntax.Document")

class DefinitionExecutable(Node["ExecutableDefinition"]):
    ...

class DefinitionTypeSystem(Node["TypeSystemDefinitionOrExtension"]):
    ...

class _DefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class Definition(metaclass=_DefinitionMeta):
    r"""DefinitionExecutable | DefinitionTypeSystem"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.Definition")
    EXECUTABLE = hydra.core.Name("executable")
    TYPE_SYSTEM = hydra.core.Name("typeSystem")

class ExecutableDocument(Node["frozenlist[ExecutableDefinition]"]):
    ...

ExecutableDocument.TYPE_ = hydra.core.Name("hydra.graphql.syntax.ExecutableDocument")

class ExecutableDefinitionOperation(Node["OperationDefinition"]):
    ...

class ExecutableDefinitionFragment(Node["FragmentDefinition"]):
    ...

class _ExecutableDefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class ExecutableDefinition(metaclass=_ExecutableDefinitionMeta):
    r"""ExecutableDefinitionOperation | ExecutableDefinitionFragment"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ExecutableDefinition")
    OPERATION = hydra.core.Name("operation")
    FRAGMENT = hydra.core.Name("fragment")

class OperationDefinitionSequence(Node["OperationDefinition_Sequence"]):
    ...

class OperationDefinitionSelectionSet(Node["SelectionSet"]):
    ...

class _OperationDefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class OperationDefinition(metaclass=_OperationDefinitionMeta):
    r"""OperationDefinitionSequence | OperationDefinitionSelectionSet"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.OperationDefinition")
    SEQUENCE = hydra.core.Name("sequence")
    SELECTION_SET = hydra.core.Name("SelectionSet")

@dataclass(frozen=True)
class OperationDefinition_Sequence:
    operation_type: OperationType
    name: Maybe[Name]
    variables_definition: Maybe[VariablesDefinition]
    directives: Maybe[Directives]
    selection_set: SelectionSet

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.OperationDefinition_Sequence")
    OPERATION_TYPE = hydra.core.Name("OperationType")
    NAME = hydra.core.Name("Name")
    VARIABLES_DEFINITION = hydra.core.Name("VariablesDefinition")
    DIRECTIVES = hydra.core.Name("Directives")
    SELECTION_SET = hydra.core.Name("SelectionSet")

class OperationType(Enum):
    QUERY = hydra.core.Name("Query")

    MUTATION = hydra.core.Name("Mutation")

    SUBSCRIPTION = hydra.core.Name("Subscription")

OperationType.TYPE_ = hydra.core.Name("hydra.graphql.syntax.OperationType")

class SelectionSet(Node["frozenlist[Selection]"]):
    ...

SelectionSet.TYPE_ = hydra.core.Name("hydra.graphql.syntax.SelectionSet")

class SelectionField(Node["Field"]):
    ...

class SelectionFragmentSpread(Node["FragmentSpread"]):
    ...

class SelectionInlineFragment(Node["InlineFragment"]):
    ...

class _SelectionMeta(type):
    def __getitem__(cls, item):
        return object

class Selection(metaclass=_SelectionMeta):
    r"""SelectionField | SelectionFragmentSpread | SelectionInlineFragment"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.Selection")
    FIELD = hydra.core.Name("Field")
    FRAGMENT_SPREAD = hydra.core.Name("FragmentSpread")
    INLINE_FRAGMENT = hydra.core.Name("InlineFragment")

@dataclass(frozen=True)
class Field:
    alias: Maybe[Alias]
    name: Name
    arguments: Maybe[Arguments]
    directives: Maybe[Directives]
    selection_set: Maybe[SelectionSet]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.Field")
    ALIAS = hydra.core.Name("Alias")
    NAME = hydra.core.Name("Name")
    ARGUMENTS = hydra.core.Name("Arguments")
    DIRECTIVES = hydra.core.Name("Directives")
    SELECTION_SET = hydra.core.Name("SelectionSet")

class AliasName(Node["Name"]):
    ...

class AliasColon:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AliasColon)
    def __hash__(self):
        return hash("AliasColon")

class _AliasMeta(type):
    def __getitem__(cls, item):
        return object

class Alias(metaclass=_AliasMeta):
    r"""AliasName | AliasColon"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.Alias")
    NAME = hydra.core.Name("Name")
    COLON = hydra.core.Name("Colon")

class Arguments(Node["frozenlist[Argument]"]):
    ...

Arguments.TYPE_ = hydra.core.Name("hydra.graphql.syntax.Arguments")

@dataclass(frozen=True)
class Argument:
    name: Name
    value: Value

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.Argument")
    NAME = hydra.core.Name("Name")
    VALUE = hydra.core.Name("Value")

@dataclass(frozen=True)
class FragmentSpread:
    fragment_name: FragmentName
    directives: Maybe[Directives]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.FragmentSpread")
    FRAGMENT_NAME = hydra.core.Name("FragmentName")
    DIRECTIVES = hydra.core.Name("Directives")

@dataclass(frozen=True)
class InlineFragment:
    type_condition: Maybe[TypeCondition]
    directives: Maybe[Directives]
    selection_set: SelectionSet

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InlineFragment")
    TYPE_CONDITION = hydra.core.Name("TypeCondition")
    DIRECTIVES = hydra.core.Name("Directives")
    SELECTION_SET = hydra.core.Name("SelectionSet")

@dataclass(frozen=True)
class FragmentDefinition:
    fragment_name: FragmentName
    type_condition: TypeCondition
    directives: Maybe[Directives]
    selection_set: SelectionSet

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.FragmentDefinition")
    FRAGMENT_NAME = hydra.core.Name("FragmentName")
    TYPE_CONDITION = hydra.core.Name("TypeCondition")
    DIRECTIVES = hydra.core.Name("Directives")
    SELECTION_SET = hydra.core.Name("SelectionSet")

class FragmentName(Node["Name"]):
    ...

FragmentName.TYPE_ = hydra.core.Name("hydra.graphql.syntax.FragmentName")

class TypeConditionOn:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeConditionOn)
    def __hash__(self):
        return hash("TypeConditionOn")

class TypeConditionNamedType(Node["NamedType"]):
    ...

class _TypeConditionMeta(type):
    def __getitem__(cls, item):
        return object

class TypeCondition(metaclass=_TypeConditionMeta):
    r"""TypeConditionOn | TypeConditionNamedType"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.TypeCondition")
    ON = hydra.core.Name("On")
    NAMED_TYPE = hydra.core.Name("NamedType")

class ValueVariable(Node["Variable"]):
    ...

class ValueInt(Node["IntValue"]):
    ...

class ValueFloat(Node["FloatValue"]):
    ...

class ValueString(Node["StringValue"]):
    ...

class ValueBoolean(Node["BooleanValue"]):
    ...

class ValueNull(Node["NullValue"]):
    ...

class ValueEnum(Node["EnumValue"]):
    ...

class ValueList(Node["ListValue"]):
    ...

class ValueObject(Node["ObjectValue"]):
    ...

class _ValueMeta(type):
    def __getitem__(cls, item):
        return object

class Value(metaclass=_ValueMeta):
    r"""ValueVariable | ValueInt | ValueFloat | ValueString | ValueBoolean | ValueNull | ValueEnum | ValueList | ValueObject"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.Value")
    VARIABLE = hydra.core.Name("Variable")
    INT = hydra.core.Name("int")
    FLOAT = hydra.core.Name("float")
    STRING = hydra.core.Name("string")
    BOOLEAN = hydra.core.Name("boolean")
    NULL = hydra.core.Name("null")
    ENUM = hydra.core.Name("enum")
    LIST = hydra.core.Name("list")
    OBJECT = hydra.core.Name("object")

class BooleanValue(Enum):
    TRUE = hydra.core.Name("True")

    FALSE = hydra.core.Name("False")

BooleanValue.TYPE_ = hydra.core.Name("hydra.graphql.syntax.BooleanValue")

class NullValue(Node[None]):
    ...

NullValue.TYPE_ = hydra.core.Name("hydra.graphql.syntax.NullValue")

class EnumValue(Node["Name"]):
    ...

EnumValue.TYPE_ = hydra.core.Name("hydra.graphql.syntax.EnumValue")

class ListValueSequence(Node["ListValue_Sequence"]):
    ...

class ListValueSequence2(Node["frozenlist[Value]"]):
    ...

class _ListValueMeta(type):
    def __getitem__(cls, item):
        return object

class ListValue(metaclass=_ListValueMeta):
    r"""ListValueSequence | ListValueSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ListValue")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class ListValue_Sequence:
    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ListValue_Sequence")

class ObjectValueSequence(Node["ObjectValue_Sequence"]):
    ...

class ObjectValueSequence2(Node["frozenlist[ObjectField]"]):
    ...

class _ObjectValueMeta(type):
    def __getitem__(cls, item):
        return object

class ObjectValue(metaclass=_ObjectValueMeta):
    r"""ObjectValueSequence | ObjectValueSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ObjectValue")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class ObjectValue_Sequence:
    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ObjectValue_Sequence")

@dataclass(frozen=True)
class ObjectField:
    name: Name
    value: Value

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ObjectField")
    NAME = hydra.core.Name("Name")
    VALUE = hydra.core.Name("Value")

@dataclass(frozen=True)
class VariablesDefinition:
    variable: Variable
    type: Type
    default_value: Maybe[DefaultValue]
    directives: Maybe[Directives]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.VariablesDefinition")
    VARIABLE = hydra.core.Name("Variable")
    TYPE = hydra.core.Name("Type")
    DEFAULT_VALUE = hydra.core.Name("DefaultValue")
    DIRECTIVES = hydra.core.Name("Directives")

class Variable(Node["Name"]):
    ...

Variable.TYPE_ = hydra.core.Name("hydra.graphql.syntax.Variable")

class DefaultValue(Node["Value"]):
    ...

DefaultValue.TYPE_ = hydra.core.Name("hydra.graphql.syntax.DefaultValue")

class TypeNamed(Node["NamedType"]):
    ...

class TypeList(Node["ListType"]):
    ...

class TypeNonNull(Node["NonNullType"]):
    ...

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

class Type(metaclass=_TypeMeta):
    r"""TypeNamed | TypeList | TypeNonNull"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.Type")
    NAMED = hydra.core.Name("named")
    LIST = hydra.core.Name("list")
    NON_NULL = hydra.core.Name("nonNull")

class NamedType(Node["Name"]):
    ...

NamedType.TYPE_ = hydra.core.Name("hydra.graphql.syntax.NamedType")

class ListType(Node["Type"]):
    ...

ListType.TYPE_ = hydra.core.Name("hydra.graphql.syntax.ListType")

class NonNullTypeNamed(Node["NamedType"]):
    ...

class NonNullTypeList(Node["ListType"]):
    ...

class _NonNullTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NonNullType(metaclass=_NonNullTypeMeta):
    r"""NonNullTypeNamed | NonNullTypeList"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.NonNullType")
    NAMED = hydra.core.Name("named")
    LIST = hydra.core.Name("list")

class Directives(Node["frozenlist[Directive]"]):
    ...

Directives.TYPE_ = hydra.core.Name("hydra.graphql.syntax.Directives")

@dataclass(frozen=True)
class Directive:
    name: Name
    arguments: Maybe[Arguments]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.Directive")
    NAME = hydra.core.Name("Name")
    ARGUMENTS = hydra.core.Name("Arguments")

class TypeSystemDocment(Node["frozenlist[TypeSystemDefinition]"]):
    ...

TypeSystemDocment.TYPE_ = hydra.core.Name("hydra.graphql.syntax.TypeSystemDocment")

class TypeSystemDefinitionSchema(Node["SchemaDefinition"]):
    ...

class TypeSystemDefinitionType(Node["TypeDefinition"]):
    ...

class TypeSystemDefinitionDirective(Node["DirectiveDefinition"]):
    ...

class _TypeSystemDefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class TypeSystemDefinition(metaclass=_TypeSystemDefinitionMeta):
    r"""TypeSystemDefinitionSchema | TypeSystemDefinitionType | TypeSystemDefinitionDirective"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.TypeSystemDefinition")
    SCHEMA = hydra.core.Name("schema")
    TYPE = hydra.core.Name("type")
    DIRECTIVE = hydra.core.Name("directive")

class TypeSystemExtensionDocument(Node["frozenlist[TypeSystemDefinitionOrExtension]"]):
    ...

TypeSystemExtensionDocument.TYPE_ = hydra.core.Name("hydra.graphql.syntax.TypeSystemExtensionDocument")

class TypeSystemDefinitionOrExtensionDefinition(Node["TypeSystemDefinition"]):
    ...

class TypeSystemDefinitionOrExtensionExtension(Node["TypeSystemExtension"]):
    ...

class _TypeSystemDefinitionOrExtensionMeta(type):
    def __getitem__(cls, item):
        return object

class TypeSystemDefinitionOrExtension(metaclass=_TypeSystemDefinitionOrExtensionMeta):
    r"""TypeSystemDefinitionOrExtensionDefinition | TypeSystemDefinitionOrExtensionExtension"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.TypeSystemDefinitionOrExtension")
    DEFINITION = hydra.core.Name("definition")
    EXTENSION = hydra.core.Name("extension")

class TypeSystemExtensionSchema(Node["SchemaExtension"]):
    ...

class TypeSystemExtensionType(Node["TypeExtension"]):
    ...

class _TypeSystemExtensionMeta(type):
    def __getitem__(cls, item):
        return object

class TypeSystemExtension(metaclass=_TypeSystemExtensionMeta):
    r"""TypeSystemExtensionSchema | TypeSystemExtensionType"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.TypeSystemExtension")
    SCHEMA = hydra.core.Name("schema")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class SchemaDefinition:
    description: Maybe[Description]
    directives: Maybe[Directives]
    root_operation_type_definition: RootOperationTypeDefinition

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.SchemaDefinition")
    DESCRIPTION = hydra.core.Name("Description")
    DIRECTIVES = hydra.core.Name("Directives")
    ROOT_OPERATION_TYPE_DEFINITION = hydra.core.Name("RootOperationTypeDefinition")

class SchemaExtensionSequence(Node["SchemaExtension_Sequence"]):
    ...

class SchemaExtensionSequence2(Node["Directives"]):
    ...

class _SchemaExtensionMeta(type):
    def __getitem__(cls, item):
        return object

class SchemaExtension(metaclass=_SchemaExtensionMeta):
    r"""SchemaExtensionSequence | SchemaExtensionSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.SchemaExtension")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class SchemaExtension_Sequence:
    directives: Maybe[Directives]
    root_operation_type_definition: RootOperationTypeDefinition

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.SchemaExtension_Sequence")
    DIRECTIVES = hydra.core.Name("Directives")
    ROOT_OPERATION_TYPE_DEFINITION = hydra.core.Name("RootOperationTypeDefinition")

@dataclass(frozen=True)
class RootOperationTypeDefinition:
    operation_type: OperationType
    named_type: NamedType

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.RootOperationTypeDefinition")
    OPERATION_TYPE = hydra.core.Name("OperationType")
    NAMED_TYPE = hydra.core.Name("NamedType")

class Description(Node["StringValue"]):
    ...

Description.TYPE_ = hydra.core.Name("hydra.graphql.syntax.Description")

class TypeDefinitionScalar(Node["ScalarTypeDefinition"]):
    ...

class TypeDefinitionObject(Node["ObjectTypeDefinition"]):
    ...

class TypeDefinitionInterface(Node["InterfaceTypeDefinition"]):
    ...

class TypeDefinitionUnion(Node["UnionTypeDefinition"]):
    ...

class TypeDefinitionEnum(Node["EnumTypeDefinition"]):
    ...

class TypeDefinitionInputObject(Node["InputObjectTypeDefinition"]):
    ...

class _TypeDefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class TypeDefinition(metaclass=_TypeDefinitionMeta):
    r"""TypeDefinitionScalar | TypeDefinitionObject | TypeDefinitionInterface | TypeDefinitionUnion | TypeDefinitionEnum | TypeDefinitionInputObject"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.TypeDefinition")
    SCALAR = hydra.core.Name("scalar")
    OBJECT = hydra.core.Name("object")
    INTERFACE = hydra.core.Name("interface")
    UNION = hydra.core.Name("union")
    ENUM = hydra.core.Name("enum")
    INPUT_OBJECT = hydra.core.Name("inputObject")

class TypeExtensionScalar(Node["ScalarTypeExtension"]):
    ...

class TypeExtensionObject(Node["ObjectTypeExtension"]):
    ...

class TypeExtensionInterface(Node["InterfaceTypeExtension"]):
    ...

class TypeExtensionUnion(Node["UnionTypeExtension"]):
    ...

class TypeExtensionEnum(Node["EnumTypeExtension"]):
    ...

class TypeExtensionInputObject(Node["InputObjectTypeExtension"]):
    ...

class _TypeExtensionMeta(type):
    def __getitem__(cls, item):
        return object

class TypeExtension(metaclass=_TypeExtensionMeta):
    r"""TypeExtensionScalar | TypeExtensionObject | TypeExtensionInterface | TypeExtensionUnion | TypeExtensionEnum | TypeExtensionInputObject"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.TypeExtension")
    SCALAR = hydra.core.Name("scalar")
    OBJECT = hydra.core.Name("object")
    INTERFACE = hydra.core.Name("interface")
    UNION = hydra.core.Name("union")
    ENUM = hydra.core.Name("enum")
    INPUT_OBJECT = hydra.core.Name("inputObject")

@dataclass(frozen=True)
class ScalarTypeDefinition:
    description: Maybe[Description]
    name: Name
    directives: Maybe[Directives]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ScalarTypeDefinition")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")

@dataclass(frozen=True)
class ScalarTypeExtension:
    name: Name
    directives: Directives

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ScalarTypeExtension")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")

@dataclass(frozen=True)
class ObjectTypeDefinition:
    description: Maybe[Description]
    name: Name
    implements_interfaces: Maybe[ImplementsInterfaces]
    directives: Maybe[Directives]
    fields_definition: Maybe[FieldsDefinition]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ObjectTypeDefinition")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")
    DIRECTIVES = hydra.core.Name("Directives")
    FIELDS_DEFINITION = hydra.core.Name("FieldsDefinition")

class ObjectTypeExtensionSequence(Node["ObjectTypeExtension_Sequence"]):
    ...

class ObjectTypeExtensionSequence2(Node["ObjectTypeExtension_Sequence2"]):
    ...

class ObjectTypeExtensionSequence3(Node["ObjectTypeExtension_Sequence3"]):
    ...

class _ObjectTypeExtensionMeta(type):
    def __getitem__(cls, item):
        return object

class ObjectTypeExtension(metaclass=_ObjectTypeExtensionMeta):
    r"""ObjectTypeExtensionSequence | ObjectTypeExtensionSequence2 | ObjectTypeExtensionSequence3"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ObjectTypeExtension")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")
    SEQUENCE3 = hydra.core.Name("sequence3")

@dataclass(frozen=True)
class ObjectTypeExtension_Sequence:
    name: Name
    implements_interfaces: Maybe[ImplementsInterfaces]
    directives: Maybe[Directives]
    fields_definition: FieldsDefinition

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ObjectTypeExtension_Sequence")
    NAME = hydra.core.Name("Name")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")
    DIRECTIVES = hydra.core.Name("Directives")
    FIELDS_DEFINITION = hydra.core.Name("FieldsDefinition")

@dataclass(frozen=True)
class ObjectTypeExtension_Sequence2:
    name: Name
    implements_interfaces: Maybe[ImplementsInterfaces]
    directives: Maybe[Directives]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ObjectTypeExtension_Sequence2")
    NAME = hydra.core.Name("Name")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")
    DIRECTIVES = hydra.core.Name("Directives")

@dataclass(frozen=True)
class ObjectTypeExtension_Sequence3:
    name: Name
    implements_interfaces: ImplementsInterfaces

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ObjectTypeExtension_Sequence3")
    NAME = hydra.core.Name("Name")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")

class ImplementsInterfacesSequence(Node["ImplementsInterfaces_Sequence"]):
    ...

class ImplementsInterfacesSequence2(Node["ImplementsInterfaces_Sequence2"]):
    ...

class _ImplementsInterfacesMeta(type):
    def __getitem__(cls, item):
        return object

class ImplementsInterfaces(metaclass=_ImplementsInterfacesMeta):
    r"""ImplementsInterfacesSequence | ImplementsInterfacesSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ImplementsInterfaces")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class ImplementsInterfaces_Sequence:
    implements_interfaces: ImplementsInterfaces
    named_type: NamedType

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ImplementsInterfaces_Sequence")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")
    NAMED_TYPE = hydra.core.Name("NamedType")

@dataclass(frozen=True)
class ImplementsInterfaces_Sequence2:
    amp: Maybe[None]
    named_type: NamedType

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.ImplementsInterfaces_Sequence2")
    AMP = hydra.core.Name("Amp")
    NAMED_TYPE = hydra.core.Name("NamedType")

class FieldsDefinition(Node["frozenlist[FieldDefinition]"]):
    ...

FieldsDefinition.TYPE_ = hydra.core.Name("hydra.graphql.syntax.FieldsDefinition")

@dataclass(frozen=True)
class FieldDefinition:
    description: Maybe[Description]
    name: Name
    arguments_definition: Maybe[ArgumentsDefinition]
    type: Type
    directives: Maybe[Directives]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.FieldDefinition")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    ARGUMENTS_DEFINITION = hydra.core.Name("ArgumentsDefinition")
    TYPE = hydra.core.Name("Type")
    DIRECTIVES = hydra.core.Name("Directives")

class ArgumentsDefinition(Node["frozenlist[InputValueDefinition]"]):
    ...

ArgumentsDefinition.TYPE_ = hydra.core.Name("hydra.graphql.syntax.ArgumentsDefinition")

@dataclass(frozen=True)
class InputValueDefinition:
    description: Maybe[Description]
    name: Name
    type: Type
    default_value: Maybe[DefaultValue]
    directives: Maybe[Directives]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InputValueDefinition")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    TYPE = hydra.core.Name("Type")
    DEFAULT_VALUE = hydra.core.Name("DefaultValue")
    DIRECTIVES = hydra.core.Name("Directives")

class InterfaceTypeDefinitionSequence(Node["InterfaceTypeDefinition_Sequence"]):
    ...

class InterfaceTypeDefinitionSequence2(Node["InterfaceTypeDefinition_Sequence2"]):
    ...

class _InterfaceTypeDefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class InterfaceTypeDefinition(metaclass=_InterfaceTypeDefinitionMeta):
    r"""InterfaceTypeDefinitionSequence | InterfaceTypeDefinitionSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InterfaceTypeDefinition")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class InterfaceTypeDefinition_Sequence:
    description: Maybe[Description]
    name: Name
    implements_interfaces: Maybe[ImplementsInterfaces]
    directives: Maybe[Directives]
    fields_definition: FieldsDefinition

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InterfaceTypeDefinition_Sequence")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")
    DIRECTIVES = hydra.core.Name("Directives")
    FIELDS_DEFINITION = hydra.core.Name("FieldsDefinition")

@dataclass(frozen=True)
class InterfaceTypeDefinition_Sequence2:
    description: Maybe[Description]
    name: Name
    implements_interfaces: ImplementsInterfaces
    directives: Maybe[Directives]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InterfaceTypeDefinition_Sequence2")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")
    DIRECTIVES = hydra.core.Name("Directives")

class InterfaceTypeExtensionSequence(Node["InterfaceTypeExtension_Sequence"]):
    ...

class InterfaceTypeExtensionSequence2(Node["InterfaceTypeExtension_Sequence2"]):
    ...

class InterfaceTypeExtensionSequence3(Node["InterfaceTypeExtension_Sequence3"]):
    ...

class _InterfaceTypeExtensionMeta(type):
    def __getitem__(cls, item):
        return object

class InterfaceTypeExtension(metaclass=_InterfaceTypeExtensionMeta):
    r"""InterfaceTypeExtensionSequence | InterfaceTypeExtensionSequence2 | InterfaceTypeExtensionSequence3"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InterfaceTypeExtension")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")
    SEQUENCE3 = hydra.core.Name("sequence3")

@dataclass(frozen=True)
class InterfaceTypeExtension_Sequence:
    name: Name
    implements_interfaces: Maybe[ImplementsInterfaces]
    directives: Maybe[Directives]
    fields_definition: FieldsDefinition

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InterfaceTypeExtension_Sequence")
    NAME = hydra.core.Name("Name")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")
    DIRECTIVES = hydra.core.Name("Directives")
    FIELDS_DEFINITION = hydra.core.Name("FieldsDefinition")

@dataclass(frozen=True)
class InterfaceTypeExtension_Sequence2:
    name: Name
    implements_interfaces: Maybe[ImplementsInterfaces]
    directives: Directives

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InterfaceTypeExtension_Sequence2")
    NAME = hydra.core.Name("Name")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")
    DIRECTIVES = hydra.core.Name("Directives")

@dataclass(frozen=True)
class InterfaceTypeExtension_Sequence3:
    name: Name
    implements_interfaces: ImplementsInterfaces

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InterfaceTypeExtension_Sequence3")
    NAME = hydra.core.Name("Name")
    IMPLEMENTS_INTERFACES = hydra.core.Name("ImplementsInterfaces")

@dataclass(frozen=True)
class UnionTypeDefinition:
    description: Maybe[Description]
    name: Name
    directives: Maybe[Directives]
    union_member_types: Maybe[UnionMemberTypes]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.UnionTypeDefinition")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")
    UNION_MEMBER_TYPES = hydra.core.Name("UnionMemberTypes")

class UnionMemberTypesSequence(Node["UnionMemberTypes_Sequence"]):
    ...

class UnionMemberTypesSequence2(Node["UnionMemberTypes_Sequence2"]):
    ...

class _UnionMemberTypesMeta(type):
    def __getitem__(cls, item):
        return object

class UnionMemberTypes(metaclass=_UnionMemberTypesMeta):
    r"""UnionMemberTypesSequence | UnionMemberTypesSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.UnionMemberTypes")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class UnionMemberTypes_Sequence:
    union_member_types: UnionMemberTypes
    named_type: NamedType

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.UnionMemberTypes_Sequence")
    UNION_MEMBER_TYPES = hydra.core.Name("UnionMemberTypes")
    NAMED_TYPE = hydra.core.Name("NamedType")

@dataclass(frozen=True)
class UnionMemberTypes_Sequence2:
    or_: Maybe[None]
    named_type: NamedType

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.UnionMemberTypes_Sequence2")
    OR = hydra.core.Name("Or")
    NAMED_TYPE = hydra.core.Name("NamedType")

class UnionTypeExtensionSequence(Node["UnionTypeExtension_Sequence"]):
    ...

class UnionTypeExtensionSequence2(Node["UnionTypeExtension_Sequence2"]):
    ...

class _UnionTypeExtensionMeta(type):
    def __getitem__(cls, item):
        return object

class UnionTypeExtension(metaclass=_UnionTypeExtensionMeta):
    r"""UnionTypeExtensionSequence | UnionTypeExtensionSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.UnionTypeExtension")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class UnionTypeExtension_Sequence:
    name: Name
    directives: Maybe[Directives]
    union_member_types: UnionMemberTypes

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.UnionTypeExtension_Sequence")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")
    UNION_MEMBER_TYPES = hydra.core.Name("UnionMemberTypes")

@dataclass(frozen=True)
class UnionTypeExtension_Sequence2:
    name: Name
    directives: Directives

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.UnionTypeExtension_Sequence2")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")

@dataclass(frozen=True)
class EnumTypeDefinition:
    description: Maybe[Description]
    name: Name
    directives: Maybe[Directives]
    enum_values_definition: Maybe[EnumValuesDefinition]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.EnumTypeDefinition")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")
    ENUM_VALUES_DEFINITION = hydra.core.Name("EnumValuesDefinition")

class EnumValuesDefinition(Node["frozenlist[EnumValueDefinition]"]):
    ...

EnumValuesDefinition.TYPE_ = hydra.core.Name("hydra.graphql.syntax.EnumValuesDefinition")

@dataclass(frozen=True)
class EnumValueDefinition:
    description: Maybe[Description]
    enum_value: EnumValue
    directives: Maybe[Directives]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.EnumValueDefinition")
    DESCRIPTION = hydra.core.Name("Description")
    ENUM_VALUE = hydra.core.Name("EnumValue")
    DIRECTIVES = hydra.core.Name("Directives")

class EnumTypeExtensionSequence(Node["EnumTypeExtension_Sequence"]):
    ...

class EnumTypeExtensionSequence2(Node["EnumTypeExtension_Sequence2"]):
    ...

class _EnumTypeExtensionMeta(type):
    def __getitem__(cls, item):
        return object

class EnumTypeExtension(metaclass=_EnumTypeExtensionMeta):
    r"""EnumTypeExtensionSequence | EnumTypeExtensionSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.EnumTypeExtension")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class EnumTypeExtension_Sequence:
    name: Name
    directives: Maybe[Directives]
    enum_values_definition: EnumValuesDefinition

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.EnumTypeExtension_Sequence")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")
    ENUM_VALUES_DEFINITION = hydra.core.Name("EnumValuesDefinition")

@dataclass(frozen=True)
class EnumTypeExtension_Sequence2:
    name: Name
    directives: Directives

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.EnumTypeExtension_Sequence2")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")

class InputObjectTypeDefinitionSequence(Node["InputObjectTypeDefinition_Sequence"]):
    ...

class InputObjectTypeDefinitionSequence2(Node["InputObjectTypeDefinition_Sequence2"]):
    ...

class _InputObjectTypeDefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class InputObjectTypeDefinition(metaclass=_InputObjectTypeDefinitionMeta):
    r"""InputObjectTypeDefinitionSequence | InputObjectTypeDefinitionSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InputObjectTypeDefinition")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class InputObjectTypeDefinition_Sequence:
    description: Maybe[Description]
    name: Name
    directives: Maybe[Directives]
    input_fields_definition: InputFieldsDefinition

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InputObjectTypeDefinition_Sequence")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")
    INPUT_FIELDS_DEFINITION = hydra.core.Name("InputFieldsDefinition")

@dataclass(frozen=True)
class InputObjectTypeDefinition_Sequence2:
    description: Maybe[Description]
    name: Name
    directives: Maybe[Directives]

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InputObjectTypeDefinition_Sequence2")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")

class InputFieldsDefinition(Node["frozenlist[InputValueDefinition]"]):
    ...

InputFieldsDefinition.TYPE_ = hydra.core.Name("hydra.graphql.syntax.InputFieldsDefinition")

class InputObjectTypeExtensionSequence(Node["InputObjectTypeExtension_Sequence"]):
    ...

class InputObjectTypeExtensionSequence2(Node["InputObjectTypeExtension_Sequence2"]):
    ...

class _InputObjectTypeExtensionMeta(type):
    def __getitem__(cls, item):
        return object

class InputObjectTypeExtension(metaclass=_InputObjectTypeExtensionMeta):
    r"""InputObjectTypeExtensionSequence | InputObjectTypeExtensionSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InputObjectTypeExtension")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class InputObjectTypeExtension_Sequence:
    name: Name
    directives: Maybe[Directives]
    input_fields_definition: InputFieldsDefinition

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InputObjectTypeExtension_Sequence")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")
    INPUT_FIELDS_DEFINITION = hydra.core.Name("InputFieldsDefinition")

@dataclass(frozen=True)
class InputObjectTypeExtension_Sequence2:
    name: Name
    directives: Directives

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.InputObjectTypeExtension_Sequence2")
    NAME = hydra.core.Name("Name")
    DIRECTIVES = hydra.core.Name("Directives")

@dataclass(frozen=True)
class DirectiveDefinition:
    description: Maybe[Description]
    name: Name
    arguments_definition: Maybe[ArgumentsDefinition]
    repeatable: Maybe[None]
    directive_locations: DirectiveLocations

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.DirectiveDefinition")
    DESCRIPTION = hydra.core.Name("Description")
    NAME = hydra.core.Name("Name")
    ARGUMENTS_DEFINITION = hydra.core.Name("ArgumentsDefinition")
    REPEATABLE = hydra.core.Name("Repeatable")
    DIRECTIVE_LOCATIONS = hydra.core.Name("DirectiveLocations")

class DirectiveLocationsSequence(Node["DirectiveLocations_Sequence"]):
    ...

class DirectiveLocationsSequence2(Node["DirectiveLocations_Sequence2"]):
    ...

class _DirectiveLocationsMeta(type):
    def __getitem__(cls, item):
        return object

class DirectiveLocations(metaclass=_DirectiveLocationsMeta):
    r"""DirectiveLocationsSequence | DirectiveLocationsSequence2"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.DirectiveLocations")
    SEQUENCE = hydra.core.Name("sequence")
    SEQUENCE2 = hydra.core.Name("sequence2")

@dataclass(frozen=True)
class DirectiveLocations_Sequence:
    directive_locations: DirectiveLocations
    directive_location: DirectiveLocation

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.DirectiveLocations_Sequence")
    DIRECTIVE_LOCATIONS = hydra.core.Name("DirectiveLocations")
    DIRECTIVE_LOCATION = hydra.core.Name("DirectiveLocation")

@dataclass(frozen=True)
class DirectiveLocations_Sequence2:
    or_: Maybe[None]
    directive_location: DirectiveLocation

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.DirectiveLocations_Sequence2")
    OR = hydra.core.Name("Or")
    DIRECTIVE_LOCATION = hydra.core.Name("DirectiveLocation")

class DirectiveLocationExecutable(Node["ExecutableDirectiveLocation"]):
    ...

class DirectiveLocationTypeSystem(Node["TypeSystemDirectiveLocation"]):
    ...

class _DirectiveLocationMeta(type):
    def __getitem__(cls, item):
        return object

class DirectiveLocation(metaclass=_DirectiveLocationMeta):
    r"""DirectiveLocationExecutable | DirectiveLocationTypeSystem"""

    TYPE_ = hydra.core.Name("hydra.graphql.syntax.DirectiveLocation")
    EXECUTABLE = hydra.core.Name("executable")
    TYPE_SYSTEM = hydra.core.Name("typeSystem")

class ExecutableDirectiveLocation(Enum):
    Q_U_E_R_Y = hydra.core.Name("QUERY")

    M_U_T_A_T_I_O_N = hydra.core.Name("MUTATION")

    S_U_B_S_C_R_I_P_T_I_O_N = hydra.core.Name("SUBSCRIPTION")

    F_I_E_L_D = hydra.core.Name("FIELD")

    F_R_A_G_M_E_N_T_D_E_F_I_N_I_T_I_O_N = hydra.core.Name("FRAGMENT_DEFINITION")

    F_R_A_G_M_E_N_T_S_P_R_E_A_D = hydra.core.Name("FRAGMENT_SPREAD")

    I_N_L_I_N_E_F_R_A_G_M_E_N_T = hydra.core.Name("INLINE_FRAGMENT")

    V_A_R_I_A_B_L_E_D_E_F_I_N_I_T_I_O_N = hydra.core.Name("VARIABLE_DEFINITION")

ExecutableDirectiveLocation.TYPE_ = hydra.core.Name("hydra.graphql.syntax.ExecutableDirectiveLocation")

class TypeSystemDirectiveLocation(Enum):
    S_C_H_E_M_A = hydra.core.Name("SCHEMA")

    S_C_A_L_A_R = hydra.core.Name("SCALAR")

    O_B_J_E_C_T = hydra.core.Name("OBJECT")

    F_I_E_L_D_D_E_F_I_N_I_T_I_O_N = hydra.core.Name("FIELD_DEFINITION")

    A_R_G_U_M_E_N_T_D_E_F_I_N_I_T_I_O_N = hydra.core.Name("ARGUMENT_DEFINITION")

    I_N_T_E_R_F_A_C_E = hydra.core.Name("INTERFACE")

    U_N_I_O_N = hydra.core.Name("UNION")

    E_N_U_M = hydra.core.Name("ENUM")

    E_N_U_M_V_A_L_U_E = hydra.core.Name("ENUM_VALUE")

    I_N_P_U_T_O_B_J_E_C_T = hydra.core.Name("INPUT_OBJECT")

    I_N_P_U_T_F_I_E_L_D_D_E_F_I_N_I_T_I_O_N = hydra.core.Name("INPUT_FIELD_DEFINITION")

TypeSystemDirectiveLocation.TYPE_ = hydra.core.Name("hydra.graphql.syntax.TypeSystemDirectiveLocation")
