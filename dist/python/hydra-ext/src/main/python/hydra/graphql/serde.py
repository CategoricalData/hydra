# Note: this is an automatically generated file. Do not edit.

r"""Serialization functions for converting GraphQL AST to abstract expressions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, Nothing
from typing import cast
import hydra.core
import hydra.graphql.syntax
import hydra.lib.lists
import hydra.lib.maybes
import hydra.serialization

def expr_name(n: hydra.graphql.syntax.Name) -> hydra.ast.Expr:
    r"""Convert a GraphQL name to an expression."""

    return hydra.serialization.cst(n.value)

def expr_enum_value(ev: hydra.graphql.syntax.EnumValue) -> hydra.ast.Expr:
    r"""Convert a GraphQL enum value to an expression."""

    return expr_name(ev.value)

def expr_description(desc: hydra.graphql.syntax.Description) -> hydra.ast.Expr:
    r"""Convert a GraphQL description to a triple-quoted comment."""

    @lru_cache(1)
    def delim() -> hydra.ast.Expr:
        return hydra.serialization.cst("\"\"\"")
    text = desc.value.value
    return hydra.serialization.newline_sep((delim(), hydra.serialization.cst(text), delim()))

def with_description(mdesc: Maybe[hydra.graphql.syntax.Description], expr: hydra.ast.Expr) -> hydra.ast.Expr:
    r"""Prepend an optional description to an expression."""

    return hydra.serialization.newline_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda x1: expr_description(x1)), mdesc), hydra.lib.maybes.pure(expr))))

def expr_enum_value_definition(def_: hydra.graphql.syntax.EnumValueDefinition) -> hydra.ast.Expr:
    r"""Convert a GraphQL enum value definition to an expression."""

    desc = def_.description
    ev = def_.enum_value
    return with_description(desc, expr_enum_value(ev))

def expr_enum_type_definition(def_: hydra.graphql.syntax.EnumTypeDefinition) -> hydra.ast.Expr:
    r"""Convert a GraphQL enum type definition to an expression."""

    desc = def_.description
    name = def_.name
    values = def_.enum_values_definition
    @lru_cache(1)
    def values_expr() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda vs: hydra.lib.lists.map((lambda x1: expr_enum_value_definition(x1)), vs.value)), values)
    return with_description(desc, hydra.serialization.space_sep((hydra.serialization.cst("enum"), expr_name(name), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.full_block_style, values_expr()))))

def expr_named_type(nt: hydra.graphql.syntax.NamedType) -> hydra.ast.Expr:
    r"""Convert a GraphQL named type to an expression."""

    return expr_name(nt.value)

def expr_list_type(lt: hydra.graphql.syntax.ListType) -> hydra.ast.Expr:
    r"""Convert a GraphQL list type to an expression."""

    return hydra.serialization.no_sep((hydra.serialization.cst("["), expr_type(lt.value), hydra.serialization.cst("]")))

def expr_non_null_type(nnt: hydra.graphql.syntax.NonNullType) -> hydra.ast.Expr:
    r"""Convert a GraphQL non-null type to an expression."""

    @lru_cache(1)
    def type_expr() -> hydra.ast.Expr:
        match nnt:
            case hydra.graphql.syntax.NonNullTypeNamed(value=nt):
                return expr_named_type(nt)

            case hydra.graphql.syntax.NonNullTypeList(value=lt):
                return expr_list_type(lt)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.no_sep((type_expr(), hydra.serialization.cst("!")))

def expr_type(typ: hydra.graphql.syntax.Type) -> hydra.ast.Expr:
    r"""Convert a GraphQL type to an expression."""

    match typ:
        case hydra.graphql.syntax.TypeNamed(value=nt):
            return expr_named_type(nt)

        case hydra.graphql.syntax.TypeList(value=lt):
            return expr_list_type(lt)

        case hydra.graphql.syntax.TypeNonNull(value=nnt):
            return expr_non_null_type(nnt)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def expr_field_definition(def_: hydra.graphql.syntax.FieldDefinition) -> hydra.ast.Expr:
    r"""Convert a GraphQL field definition to an expression."""

    desc = def_.description
    name = def_.name
    typ = def_.type
    @lru_cache(1)
    def name_part() -> hydra.ast.Expr:
        return hydra.serialization.no_sep((expr_name(name), hydra.serialization.cst(":")))
    @lru_cache(1)
    def type_part() -> hydra.ast.Expr:
        return expr_type(typ)
    return with_description(desc, hydra.serialization.space_sep((name_part(), type_part())))

def expr_object_type_definition(def_: hydra.graphql.syntax.ObjectTypeDefinition) -> hydra.ast.Expr:
    r"""Convert a GraphQL object type definition to an expression."""

    desc = def_.description
    name = def_.name
    fields = def_.fields_definition
    @lru_cache(1)
    def fields_expr() -> frozenlist[hydra.ast.Expr]:
        return hydra.lib.maybes.maybe((lambda : ()), (lambda fs: hydra.lib.lists.map((lambda x1: expr_field_definition(x1)), fs.value)), fields)
    return with_description(desc, hydra.serialization.space_sep((hydra.serialization.cst("type"), expr_name(name), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.full_block_style, fields_expr()))))

def expr_type_definition(def_: hydra.graphql.syntax.TypeDefinition) -> hydra.ast.Expr:
    r"""Convert a GraphQL type definition to an expression."""

    match def_:
        case hydra.graphql.syntax.TypeDefinitionScalar():
            return hydra.serialization.cst("Unsupported: scalar type definition")

        case hydra.graphql.syntax.TypeDefinitionObject(value=od):
            return expr_object_type_definition(od)

        case hydra.graphql.syntax.TypeDefinitionInterface():
            return hydra.serialization.cst("Unsupported: interface type definition")

        case hydra.graphql.syntax.TypeDefinitionUnion():
            return hydra.serialization.cst("Unsupported: union type definition")

        case hydra.graphql.syntax.TypeDefinitionEnum(value=ed):
            return expr_enum_type_definition(ed)

        case hydra.graphql.syntax.TypeDefinitionInputObject():
            return hydra.serialization.cst("Unsupported: input object type definition")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def expr_type_system_definition(def_: hydra.graphql.syntax.TypeSystemDefinition) -> hydra.ast.Expr:
    r"""Convert a GraphQL type system definition to an expression."""

    match def_:
        case hydra.graphql.syntax.TypeSystemDefinitionSchema():
            return hydra.serialization.cst("Unsupported: schema definition")

        case hydra.graphql.syntax.TypeSystemDefinitionType(value=dt):
            return expr_type_definition(dt)

        case hydra.graphql.syntax.TypeSystemDefinitionDirective():
            return hydra.serialization.cst("Unsupported: directive definition")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def expr_type_system_definition_or_extension(de: hydra.graphql.syntax.TypeSystemDefinitionOrExtension) -> hydra.ast.Expr:
    r"""Convert a GraphQL type system definition or extension to an expression."""

    match de:
        case hydra.graphql.syntax.TypeSystemDefinitionOrExtensionDefinition(value=d):
            return expr_type_system_definition(d)

        case hydra.graphql.syntax.TypeSystemDefinitionOrExtensionExtension():
            return hydra.serialization.cst("Unsupported: type system extension")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def expr_definition(def_: hydra.graphql.syntax.Definition) -> hydra.ast.Expr:
    r"""Convert a GraphQL definition to an expression."""

    match def_:
        case hydra.graphql.syntax.DefinitionExecutable():
            return hydra.serialization.cst("Unsupported: executable definition")

        case hydra.graphql.syntax.DefinitionTypeSystem(value=de):
            return expr_type_system_definition_or_extension(de)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def expr_document(d: hydra.graphql.syntax.Document) -> hydra.ast.Expr:
    r"""Convert a GraphQL document to an expression."""

    return hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: expr_definition(x1)), d.value))
