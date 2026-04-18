# Note: this is an automatically generated file. Do not edit.

r"""Serialization functions for converting Pegasus PDL AST to abstract expressions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, Nothing
from typing import cast
import hydra.core
import hydra.formatting
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.pegasus.pdl
import hydra.serialization

def expr_annotations(anns: hydra.pegasus.pdl.Annotations) -> Maybe[hydra.ast.Expr]:
    r"""Convert PDL annotations to an optional expression (doc comment)."""

    d = anns.doc
    return hydra.lib.maybes.map((lambda s: hydra.serialization.cst(hydra.formatting.java_style_comment(s))), d)

def with_annotations(anns: hydra.pegasus.pdl.Annotations, expr: hydra.ast.Expr) -> hydra.ast.Expr:
    r"""Prepend annotations (doc comment) to an expression."""

    return hydra.serialization.newline_sep(hydra.lib.maybes.cat((expr_annotations(anns), hydra.lib.maybes.pure(expr))))

def expr_enum_field(ef: hydra.pegasus.pdl.EnumField) -> hydra.ast.Expr:
    r"""Convert a PDL enum field to an expression."""

    name = ef.name.value
    anns = ef.annotations
    return with_annotations(anns, hydra.serialization.cst(name))

def expr_qualified_name(qn: hydra.pegasus.pdl.QualifiedName) -> hydra.ast.Expr:
    r"""Convert a qualified name to an expression."""

    name = qn.name.value
    ns = qn.namespace
    @lru_cache(1)
    def parts() -> frozenlist[str]:
        return hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda n: n.value), ns), hydra.lib.maybes.pure(name)))
    return hydra.serialization.cst(hydra.lib.strings.intercalate(".", parts()))

def expr_import(qn: hydra.pegasus.pdl.QualifiedName) -> hydra.ast.Expr:
    r"""Convert a qualified name to an import expression."""

    return hydra.serialization.space_sep((hydra.serialization.cst("import"), expr_qualified_name(qn)))

def expr_primitive_type(pt: hydra.pegasus.pdl.PrimitiveType):
    def _hoist_hydra_pegasus_serde_expr_primitive_type_1(v1):
        match v1:
            case hydra.pegasus.pdl.PrimitiveType.BOOLEAN:
                return "boolean"

            case hydra.pegasus.pdl.PrimitiveType.BYTES:
                return "bytes"

            case hydra.pegasus.pdl.PrimitiveType.DOUBLE:
                return "double"

            case hydra.pegasus.pdl.PrimitiveType.FLOAT:
                return "float"

            case hydra.pegasus.pdl.PrimitiveType.INT:
                return "int"

            case hydra.pegasus.pdl.PrimitiveType.LONG:
                return "long"

            case hydra.pegasus.pdl.PrimitiveType.STRING:
                return "string"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.cst(_hoist_hydra_pegasus_serde_expr_primitive_type_1(pt))

def expr_schema(schema: hydra.pegasus.pdl.Schema) -> hydra.ast.Expr:
    r"""Convert a schema to an expression."""

    match schema:
        case hydra.pegasus.pdl.SchemaArray(value=s):
            return hydra.serialization.no_sep((hydra.serialization.cst("array"), hydra.serialization.bracket_list(hydra.serialization.inline_style, (expr_schema(s),))))

        case hydra.pegasus.pdl.SchemaMap(value=s2):
            return hydra.serialization.no_sep((hydra.serialization.cst("map"), hydra.serialization.bracket_list(hydra.serialization.inline_style, (hydra.serialization.cst("string"), expr_schema(s2)))))

        case hydra.pegasus.pdl.SchemaNamed(value=qn):
            return expr_qualified_name(qn)

        case hydra.pegasus.pdl.SchemaNull():
            return hydra.serialization.cst("null")

        case hydra.pegasus.pdl.SchemaPrimitive(value=pt):
            return expr_primitive_type(pt)

        case hydra.pegasus.pdl.SchemaUnion(value=us):
            return hydra.serialization.no_sep((hydra.serialization.cst("union"), hydra.serialization.bracket_list(hydra.serialization.full_block_style, hydra.lib.lists.map((lambda x1: expr_union_member(x1)), us.value))))

        case _:
            raise TypeError("Unsupported Schema")

def expr_union_member(um: hydra.pegasus.pdl.UnionMember) -> hydra.ast.Expr:
    r"""Convert a union member to an expression."""

    alias = um.alias
    schema = um.value
    anns = um.annotations
    return with_annotations(anns, hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.map((lambda fn: hydra.serialization.cst(hydra.lib.strings.cat2(fn.value, ":"))), alias), hydra.lib.maybes.pure(expr_schema(schema))))))

def expr_record_field(rf: hydra.pegasus.pdl.RecordField) -> hydra.ast.Expr:
    r"""Convert a record field to an expression."""

    name = rf.name.value
    schema = rf.value
    optional = rf.optional
    anns = rf.annotations
    return with_annotations(anns, hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(hydra.serialization.cst(hydra.lib.strings.cat2(name, ":"))), hydra.lib.logic.if_else(optional, (lambda : hydra.lib.maybes.pure(hydra.serialization.cst("optional"))), (lambda : Nothing())), hydra.lib.maybes.pure(expr_schema(schema))))))

def expr_named_schema(ns: hydra.pegasus.pdl.NamedSchema):
    r"""Convert a named schema to an expression."""

    qn = ns.qualified_name
    t = ns.type
    anns = ns.annotations
    def _hoist_qn_body_1(v1):
        match v1:
            case hydra.pegasus.pdl.NamedSchemaTypeRecord(value=rs):
                fields = rs.fields
                return hydra.serialization.space_sep((hydra.serialization.cst("record"), expr_qualified_name(qn), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.full_block_style, hydra.lib.lists.map((lambda x1: expr_record_field(x1)), fields))))

            case hydra.pegasus.pdl.NamedSchemaTypeEnum(value=es):
                fields = es.fields
                return hydra.serialization.space_sep((hydra.serialization.cst("enum"), expr_qualified_name(qn), hydra.serialization.curly_braces_list(Nothing(), hydra.serialization.full_block_style, hydra.lib.lists.map((lambda x1: expr_enum_field(x1)), fields))))

            case hydra.pegasus.pdl.NamedSchemaTypeTyperef(value=schema):
                return hydra.serialization.space_sep((hydra.serialization.cst("typeref"), expr_qualified_name(qn), hydra.serialization.cst("="), expr_schema(schema)))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return with_annotations(anns, _hoist_qn_body_1(t))

def expr_schema_file(sf: hydra.pegasus.pdl.SchemaFile) -> hydra.ast.Expr:
    r"""Convert a schema file to an expression."""

    ns = sf.namespace.value
    pkg = sf.package
    imports = sf.imports
    schemas = sf.schemas
    @lru_cache(1)
    def namespace_sec() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.pure(hydra.serialization.space_sep((hydra.serialization.cst("namespace"), hydra.serialization.cst(ns))))
    @lru_cache(1)
    def package_sec() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.map((lambda p: hydra.serialization.space_sep((hydra.serialization.cst("package"), hydra.serialization.cst(p.value)))), pkg)
    @lru_cache(1)
    def imports_sec() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(imports), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: expr_import(x1)), imports)))))
    @lru_cache(1)
    def schema_secs() -> frozenlist[Maybe[hydra.ast.Expr]]:
        return hydra.lib.lists.map((lambda s: hydra.lib.maybes.pure(expr_named_schema(s))), schemas)
    return hydra.serialization.double_newline_sep(hydra.lib.maybes.cat(hydra.lib.lists.concat(((namespace_sec(), package_sec(), imports_sec()), schema_secs()))))
