# Note: this is an automatically generated file. Do not edit.

r"""Serialization functions for converting Protocol Buffers v3 AST to abstract expressions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Maybe, Nothing, frozenlist
from typing import cast
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maybes
import hydra.lib.strings
import hydra.protobuf.proto3
import hydra.serialization

# The name of the deprecated option.
deprecated_option_name = "deprecated"

# A special Protobuf option name for descriptions (documentation).
description_option_name = "_description"

def exclude_internal_options(opts: frozenlist[hydra.protobuf.proto3.Option]) -> frozenlist[hydra.protobuf.proto3.Option]:
    r"""Filter out internal options (those whose names start with underscore)."""

    return hydra.lib.lists.filter((lambda opt: hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.lib.maybes.from_maybe((lambda : 0), hydra.lib.strings.maybe_char_at(0, opt.name)), 95))), opts)

def opt_desc(double_newline: bool, opts: frozenlist[hydra.protobuf.proto3.Option], expr: hydra.ast.Expr) -> hydra.ast.Expr:
    r"""Prepend an optional description comment to an expression."""

    @lru_cache(1)
    def descs() -> frozenlist[hydra.protobuf.proto3.Option]:
        return hydra.lib.lists.filter((lambda opt: hydra.lib.equality.equal(opt.name, "_description")), opts)
    return hydra.lib.maybes.maybe((lambda : expr), (lambda first_desc: (desc_value := first_desc.value, desc_str := (_hoist_desc_str_1 := (lambda v1: (lambda b: hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false")))(v1.value) if isinstance(v1, hydra.protobuf.proto3.ValueBoolean) else (lambda s: s)(v1.value) if isinstance(v1, hydra.protobuf.proto3.ValueString) else hydra.dsl.python.unsupported("no matching case in inline union elimination")), _hoist_desc_str_1(desc_value))[1], comment_lines := hydra.lib.lists.map((lambda line: hydra.lib.strings.cat2("// ", line)), hydra.lib.strings.lines(desc_str)), comment := hydra.serialization.cst(hydra.lib.strings.intercalate("\n", comment_lines)), sep := hydra.lib.logic.if_else(double_newline, (lambda : hydra.serialization.double_newline_sep((comment, expr))), (lambda : hydra.serialization.newline_sep((comment, expr)))), sep)[5]), hydra.lib.lists.maybe_head(descs()))

def proto_block(exprs: frozenlist[hydra.ast.Expr]) -> hydra.ast.Expr:
    r"""Wrap expressions in a curly-braced block with double-newline separation."""

    return hydra.serialization.brackets(hydra.serialization.curly_braces, hydra.serialization.full_block_style, hydra.serialization.double_newline_sep(exprs))

def semi(e: hydra.ast.Expr) -> hydra.ast.Expr:
    r"""Append a semicolon to an expression."""

    return hydra.serialization.no_sep((e, hydra.serialization.cst(";")))

def write_enum_value(ev: hydra.protobuf.proto3.EnumValue) -> hydra.ast.Expr:
    r"""Convert an enum value to an expression."""

    name = ev.name
    number = ev.number
    options = ev.options
    return opt_desc(False, options, semi(hydra.serialization.space_sep((hydra.serialization.cst(name.value), hydra.serialization.cst("="), hydra.serialization.cst(hydra.lib.literals.show_int32(number))))))

def write_enum_definition(ed: hydra.protobuf.proto3.EnumDefinition) -> hydra.ast.Expr:
    r"""Convert an enum definition to an expression."""

    name = ed.name
    values = ed.values
    options = ed.options
    return opt_desc(False, options, hydra.serialization.space_sep((hydra.serialization.cst("enum"), hydra.serialization.cst(name.value), proto_block(hydra.lib.lists.map((lambda x1: write_enum_value(x1)), values)))))

def write_value(v: hydra.protobuf.proto3.Value):
    def _hoist_hydra_protobuf_serde_write_value_1(v1):
        match v1:
            case hydra.protobuf.proto3.ValueBoolean(value=b):
                return hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false"))

            case hydra.protobuf.proto3.ValueString(value=s):
                return hydra.lib.literals.show_string(s)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.cst(_hoist_hydra_protobuf_serde_write_value_1(v))

def write_field_option(opt: hydra.protobuf.proto3.Option) -> hydra.ast.Expr:
    r"""Convert a field option to an expression."""

    name = opt.name
    value = opt.value
    return hydra.serialization.space_sep((hydra.serialization.cst(name), hydra.serialization.cst("="), write_value(value)))

def write_field_options(opts0: frozenlist[hydra.protobuf.proto3.Option]) -> Maybe[hydra.ast.Expr]:
    r"""Convert field options to an optional bracket-enclosed expression."""

    @lru_cache(1)
    def opts() -> frozenlist[hydra.protobuf.proto3.Option]:
        return exclude_internal_options(opts0)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(opts()), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.bracket_list(hydra.serialization.inline_style, hydra.lib.lists.map((lambda x1: write_field_option(x1)), opts())))))

def write_scalar_type(sct: hydra.protobuf.proto3.ScalarType):
    def _hoist_hydra_protobuf_serde_write_scalar_type_1(v1):
        match v1:
            case hydra.protobuf.proto3.ScalarType.BOOL:
                return "bool"

            case hydra.protobuf.proto3.ScalarType.BYTES:
                return "bytes"

            case hydra.protobuf.proto3.ScalarType.DOUBLE:
                return "double"

            case hydra.protobuf.proto3.ScalarType.FIXED32:
                return "fixed32"

            case hydra.protobuf.proto3.ScalarType.FIXED64:
                return "fixed64"

            case hydra.protobuf.proto3.ScalarType.FLOAT:
                return "float"

            case hydra.protobuf.proto3.ScalarType.INT32:
                return "int32"

            case hydra.protobuf.proto3.ScalarType.INT64:
                return "int64"

            case hydra.protobuf.proto3.ScalarType.SFIXED32:
                return "sfixed32"

            case hydra.protobuf.proto3.ScalarType.SFIXED64:
                return "sfixed64"

            case hydra.protobuf.proto3.ScalarType.SINT32:
                return "sint32"

            case hydra.protobuf.proto3.ScalarType.SINT64:
                return "sint64"

            case hydra.protobuf.proto3.ScalarType.STRING:
                return "string"

            case hydra.protobuf.proto3.ScalarType.UINT32:
                return "uint32"

            case hydra.protobuf.proto3.ScalarType.UINT64:
                return "uint64"

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.serialization.cst(_hoist_hydra_protobuf_serde_write_scalar_type_1(sct))

def write_simple_type(st: hydra.protobuf.proto3.SimpleType) -> hydra.ast.Expr:
    r"""Convert a simple type to an expression."""

    match st:
        case hydra.protobuf.proto3.SimpleTypeReference(value=name):
            return hydra.serialization.cst(name.value)

        case hydra.protobuf.proto3.SimpleTypeScalar(value=sct):
            return write_scalar_type(sct)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_field_type(ftyp: hydra.protobuf.proto3.FieldType) -> hydra.ast.Expr:
    r"""Convert a field type to an expression."""

    match ftyp:
        case hydra.protobuf.proto3.FieldTypeMap(value=mt):
            kt = mt.keys
            vt = mt.values
            return hydra.serialization.no_sep((hydra.serialization.cst("map"), hydra.serialization.angle_braces_list(hydra.serialization.inline_style, (write_simple_type(kt), write_simple_type(vt)))))

        case hydra.protobuf.proto3.FieldTypeRepeated(value=st):
            return hydra.serialization.space_sep((hydra.serialization.cst("repeated"), write_simple_type(st)))

        case hydra.protobuf.proto3.FieldTypeSimple(value=st2):
            return write_simple_type(st2)

        case hydra.protobuf.proto3.FieldTypeOneof():
            return hydra.serialization.cst("oneof")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_field(f: hydra.protobuf.proto3.Field):
    r"""Convert a field to an expression."""

    name = f.name
    typ = f.type
    num = f.number
    options = f.options
    def _hoist_name_body_1(v1):
        match v1:
            case hydra.protobuf.proto3.FieldTypeOneof(value=fields):
                return hydra.serialization.space_sep((hydra.serialization.cst("oneof"), hydra.serialization.cst(name.value), proto_block(hydra.lib.lists.map((lambda x1: write_field(x1)), fields))))

            case hydra.protobuf.proto3.FieldTypeMap(value=mt):
                kt = mt.keys
                vt = mt.values
                return semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(write_field_type(typ)), hydra.lib.maybes.pure(hydra.serialization.cst(name.value)), hydra.lib.maybes.pure(hydra.serialization.cst("=")), hydra.lib.maybes.pure(hydra.serialization.cst(hydra.lib.literals.show_int32(num))), write_field_options(options)))))

            case hydra.protobuf.proto3.FieldTypeRepeated():
                return semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(write_field_type(typ)), hydra.lib.maybes.pure(hydra.serialization.cst(name.value)), hydra.lib.maybes.pure(hydra.serialization.cst("=")), hydra.lib.maybes.pure(hydra.serialization.cst(hydra.lib.literals.show_int32(num))), write_field_options(options)))))

            case hydra.protobuf.proto3.FieldTypeSimple():
                return semi(hydra.serialization.space_sep(hydra.lib.maybes.cat((hydra.lib.maybes.pure(write_field_type(typ)), hydra.lib.maybes.pure(hydra.serialization.cst(name.value)), hydra.lib.maybes.pure(hydra.serialization.cst("=")), hydra.lib.maybes.pure(hydra.serialization.cst(hydra.lib.literals.show_int32(num))), write_field_options(options)))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return opt_desc(False, options, _hoist_name_body_1(typ))

def write_message_definition(md: hydra.protobuf.proto3.MessageDefinition) -> hydra.ast.Expr:
    r"""Convert a message definition to an expression."""

    name = md.name
    fields = md.fields
    options = md.options
    return opt_desc(False, options, hydra.serialization.space_sep((hydra.serialization.cst("message"), hydra.serialization.cst(name.value), proto_block(hydra.lib.lists.map((lambda x1: write_field(x1)), fields)))))

def write_definition(def_: hydra.protobuf.proto3.Definition) -> hydra.ast.Expr:
    r"""Convert a definition to an expression."""

    match def_:
        case hydra.protobuf.proto3.DefinitionEnum(value=e):
            return write_enum_definition(e)

        case hydra.protobuf.proto3.DefinitionMessage(value=m):
            return write_message_definition(m)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def write_file_option(opt: hydra.protobuf.proto3.Option) -> hydra.ast.Expr:
    r"""Convert a file-level option to an expression."""

    name = opt.name
    value = opt.value
    return semi(hydra.serialization.space_sep((hydra.serialization.cst("option"), hydra.serialization.cst(name), hydra.serialization.cst("="), write_value(value))))

def write_file_options(opts0: frozenlist[hydra.protobuf.proto3.Option]) -> Maybe[hydra.ast.Expr]:
    r"""Convert file-level options to an optional newline-separated expression."""

    @lru_cache(1)
    def opts() -> frozenlist[hydra.protobuf.proto3.Option]:
        return exclude_internal_options(opts0)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(opts()), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_file_option(x1)), opts())))))

def write_import(ref: hydra.protobuf.proto3.FileReference) -> hydra.ast.Expr:
    r"""Convert a file reference to an import expression."""

    return semi(hydra.serialization.space_sep((hydra.serialization.cst("import"), hydra.serialization.cst(hydra.lib.literals.show_string(ref.value)))))

def write_proto_file(pf: hydra.protobuf.proto3.ProtoFile) -> hydra.ast.Expr:
    r"""Convert a proto file to an expression."""

    pkg = pf.package
    imports = pf.imports
    defs = pf.types
    options = pf.options
    @lru_cache(1)
    def header_sec() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.maybes.pure(hydra.serialization.newline_sep((semi(hydra.serialization.cst("syntax = \"proto3\"")), semi(hydra.serialization.space_sep((hydra.serialization.cst("package"), hydra.serialization.cst(pkg.value)))))))
    @lru_cache(1)
    def imports_sec() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(imports), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.newline_sep(hydra.lib.lists.map((lambda x1: write_import(x1)), imports)))))
    @lru_cache(1)
    def options1() -> frozenlist[hydra.protobuf.proto3.Option]:
        return hydra.lib.lists.filter((lambda opt: hydra.lib.logic.not_(hydra.lib.equality.equal(opt.name, "_description"))), options)
    @lru_cache(1)
    def options_sec() -> Maybe[hydra.ast.Expr]:
        return write_file_options(options1())
    @lru_cache(1)
    def defs_sec() -> Maybe[hydra.ast.Expr]:
        return hydra.lib.logic.if_else(hydra.lib.lists.null(defs), (lambda : Nothing()), (lambda : hydra.lib.maybes.pure(hydra.serialization.double_newline_sep(hydra.lib.lists.map((lambda x1: write_definition(x1)), defs)))))
    return opt_desc(True, options, hydra.serialization.double_newline_sep(hydra.lib.maybes.cat((header_sec(), imports_sec(), options_sec(), defs_sec()))))
