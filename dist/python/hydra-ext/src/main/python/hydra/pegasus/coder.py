# Note: this is an automatically generated file. Do not edit.

r"""Pegasus PDL code generator: converts Hydra modules to PDL schema files."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.analysis
import hydra.annotations
import hydra.core
import hydra.dependencies
import hydra.environment
import hydra.errors
import hydra.formatting
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.pegasus.pdl
import hydra.pegasus.serde
import hydra.serialization
import hydra.show.core
import hydra.strip
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def slashes_to_dots(s: str) -> str:
    r"""Replace all forward slashes with dots in a string."""

    return hydra.lib.strings.intercalate(".", hydra.lib.strings.split_on("/", s))

def pdl_name_for_module(mod: hydra.packaging.Module) -> hydra.pegasus.pdl.Namespace:
    r"""Convert a module's namespace to a PDL namespace."""

    return hydra.pegasus.pdl.Namespace(slashes_to_dots(mod.namespace.value))

def to_pair(mod: hydra.packaging.Module, aliases: T0, schema_pair: tuple[hydra.pegasus.pdl.NamedSchema, frozenlist[hydra.pegasus.pdl.QualifiedName]]) -> tuple[str, hydra.pegasus.pdl.SchemaFile]:
    @lru_cache(1)
    def schema() -> hydra.pegasus.pdl.NamedSchema:
        return hydra.lib.pairs.first(schema_pair)
    @lru_cache(1)
    def imports() -> frozenlist[hydra.pegasus.pdl.QualifiedName]:
        return hydra.lib.pairs.second(schema_pair)
    @lru_cache(1)
    def ns_() -> hydra.pegasus.pdl.Namespace:
        return pdl_name_for_module(mod)
    local = schema().qualified_name.name.value
    @lru_cache(1)
    def path() -> str:
        return hydra.names.namespace_to_file_path(hydra.util.CaseConvention.CAMEL, hydra.packaging.FileExtension("pdl"), hydra.packaging.Namespace(hydra.lib.strings.cat2(mod.namespace.value, hydra.lib.strings.cat2("/", local))))
    return (path(), hydra.pegasus.pdl.SchemaFile(ns_(), Nothing(), imports(), (schema(),)))

def doc(s: Maybe[str]) -> hydra.pegasus.pdl.Annotations:
    r"""Create PDL annotations from an optional doc string."""

    return hydra.pegasus.pdl.Annotations(s, False)

def get_anns(cx: T0, g: hydra.graph.Graph, typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.pegasus.pdl.Annotations]:
    return hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, g, typ), (lambda r: Right(doc(r))))

def encode_enum_field(cx: T0, g: hydra.graph.Graph, ft: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.pegasus.pdl.EnumField]:
    name = ft.name
    typ = ft.type
    return hydra.lib.eithers.bind(get_anns(cx, g, typ), (lambda anns: Right(hydra.pegasus.pdl.EnumField(hydra.pegasus.pdl.EnumFieldName(hydra.formatting.convert_case(hydra.util.CaseConvention.CAMEL, hydra.util.CaseConvention.UPPER_SNAKE, name.value)), anns))))

# Empty PDL annotations.
no_annotations = hydra.pegasus.pdl.Annotations(Nothing(), False)

def simple_union_member(schema: hydra.pegasus.pdl.Schema) -> hydra.pegasus.pdl.UnionMember:
    r"""Create a simple union member without an alias."""

    return hydra.pegasus.pdl.UnionMember(Nothing(), schema, no_annotations)

def pdl_name_for_element(aliases: FrozenDict[hydra.packaging.Namespace, str], with_ns: bool, name: hydra.core.Name) -> hydra.pegasus.pdl.QualifiedName:
    r"""Convert a Hydra element name to a PDL qualified name."""

    @lru_cache(1)
    def qn() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    ns_ = qn().namespace
    local = qn().local
    @lru_cache(1)
    def alias() -> Maybe[str]:
        return hydra.lib.maybes.bind(ns_, (lambda n: hydra.lib.maps.lookup(n, aliases)))
    return hydra.pegasus.pdl.QualifiedName(hydra.pegasus.pdl.Name(local), hydra.lib.logic.if_else(with_ns, (lambda : hydra.lib.maybes.map((lambda a: hydra.pegasus.pdl.Namespace(a)), alias())), (lambda : Nothing())))

def encode(cx: T0, g: hydra.graph.Graph, aliases: FrozenDict[hydra.packaging.Namespace, str], t: hydra.core.Type):
    match hydra.strip.deannotate_type(t):
        case hydra.core.TypeRecord(value=rt):
            return hydra.lib.logic.if_else(hydra.lib.lists.null(rt), (lambda : encode(cx, g, aliases, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(hydra.core.IntegerType.INT32)))))), (lambda : hydra.lib.eithers.bind(encode_type(cx, g, aliases, t), (lambda res: hydra.lib.eithers.either((lambda schema: Right(schema)), (lambda _: Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("type resolved to an unsupported nested named schema: ", hydra.show.core.type(t))))))), res)))))

        case _:
            return hydra.lib.eithers.bind(encode_type(cx, g, aliases, t), (lambda res: hydra.lib.eithers.either((lambda schema: Right(schema)), (lambda _: Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("type resolved to an unsupported nested named schema: ", hydra.show.core.type(t))))))), res)))

def encode_possibly_optional_type(cx: T0, g: hydra.graph.Graph, aliases: FrozenDict[hydra.packaging.Namespace, str], typ: hydra.core.Type) -> Either[hydra.errors.Error, tuple[hydra.pegasus.pdl.Schema, bool]]:
    while True:
        match hydra.strip.deannotate_type(typ):
            case hydra.core.TypeMaybe(value=ot):
                return hydra.lib.eithers.bind(encode(cx, g, aliases, ot), (lambda t: Right((t, True))))

            case hydra.core.TypeRecord():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeUnion():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeLiteral():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeList():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeMap():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeSet():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeVariable():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeWrap():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeEither():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypePair():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeVoid():
                return hydra.lib.eithers.bind(encode(cx, g, aliases, typ), (lambda t: Right((t, False))))

            case hydra.core.TypeAnnotated(value=at):
                cx = cx
                g = g
                aliases = aliases
                typ = at.body
                continue

            case _:
                raise TypeError("Unsupported Type")

def encode_record_field(cx: T0, g: hydra.graph.Graph, aliases: FrozenDict[hydra.packaging.Namespace, str], ft: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.pegasus.pdl.RecordField]:
    name = ft.name
    typ = ft.type
    return hydra.lib.eithers.bind(get_anns(cx, g, typ), (lambda anns: hydra.lib.eithers.bind(encode_possibly_optional_type(cx, g, aliases, typ), (lambda opt_result: (schema := hydra.lib.pairs.first(opt_result), optional := hydra.lib.pairs.second(opt_result), Right(hydra.pegasus.pdl.RecordField(hydra.pegasus.pdl.FieldName(name.value), schema, optional, Nothing(), anns)))[2]))))

def encode_type(cx: T0, g: hydra.graph.Graph, aliases: FrozenDict[hydra.packaging.Namespace, str], typ: hydra.core.Type):
    while True:
        def _hoist_hydra_pegasus_coder_encode_type_1(typ, v1):
            match v1:
                case hydra.core.FloatType.FLOAT32:
                    return Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaPrimitive(hydra.pegasus.pdl.PrimitiveType.FLOAT))))

                case hydra.core.FloatType.FLOAT64:
                    return Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaPrimitive(hydra.pegasus.pdl.PrimitiveType.DOUBLE))))

                case _:
                    return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected ", hydra.lib.strings.cat2("float32 or float64", hydra.lib.strings.cat2(", found: ", hydra.show.core.type(typ))))))))
        def _hoist_hydra_pegasus_coder_encode_type_2(typ, v1):
            match v1:
                case hydra.core.IntegerType.INT32:
                    return Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaPrimitive(hydra.pegasus.pdl.PrimitiveType.INT))))

                case hydra.core.IntegerType.INT64:
                    return Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaPrimitive(hydra.pegasus.pdl.PrimitiveType.LONG))))

                case _:
                    return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected ", hydra.lib.strings.cat2("int32 or int64", hydra.lib.strings.cat2(", found: ", hydra.show.core.type(typ))))))))
        def _hoist_hydra_pegasus_coder_encode_type_3(typ, v1):
            match v1:
                case hydra.core.LiteralTypeBinary():
                    return Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaPrimitive(hydra.pegasus.pdl.PrimitiveType.BYTES))))

                case hydra.core.LiteralTypeBoolean():
                    return Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaPrimitive(hydra.pegasus.pdl.PrimitiveType.BOOLEAN))))

                case hydra.core.LiteralTypeFloat(value=ft):
                    return _hoist_hydra_pegasus_coder_encode_type_1(typ, ft)

                case hydra.core.LiteralTypeInteger(value=it):
                    return _hoist_hydra_pegasus_coder_encode_type_2(typ, it)

                case hydra.core.LiteralTypeString():
                    return Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaPrimitive(hydra.pegasus.pdl.PrimitiveType.STRING))))

                case _:
                    return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected ", hydra.lib.strings.cat2("PDL-supported literal type", hydra.lib.strings.cat2(", found: ", hydra.show.core.type(typ))))))))
        match typ:
            case hydra.core.TypeAnnotated(value=at):
                cx = cx
                g = g
                aliases = aliases
                typ = at.body
                continue

            case hydra.core.TypeEither(value=et):
                return hydra.lib.eithers.bind(encode(cx, g, aliases, et.left), (lambda left_schema: hydra.lib.eithers.bind(encode(cx, g, aliases, et.right), (lambda right_schema: (left_member := hydra.pegasus.pdl.UnionMember(Just(hydra.pegasus.pdl.FieldName("left")), left_schema, no_annotations), right_member := hydra.pegasus.pdl.UnionMember(Just(hydra.pegasus.pdl.FieldName("right")), right_schema, no_annotations), Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaUnion(hydra.pegasus.pdl.UnionSchema((left_member, right_member)))))))[2]))))

            case hydra.core.TypeList(value=lt):
                return hydra.lib.eithers.bind(encode(cx, g, aliases, lt), (lambda inner: Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaArray(inner))))))

            case hydra.core.TypeLiteral(value=lt2):
                return _hoist_hydra_pegasus_coder_encode_type_3(typ, lt2)

            case hydra.core.TypeMap(value=mt):
                return hydra.lib.eithers.bind(encode(cx, g, aliases, mt.values), (lambda inner: Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaMap(inner))))))

            case hydra.core.TypePair(value=pt):
                return hydra.lib.eithers.bind(encode(cx, g, aliases, pt.first), (lambda first_schema: hydra.lib.eithers.bind(encode(cx, g, aliases, pt.second), (lambda second_schema: (first_field := hydra.pegasus.pdl.RecordField(hydra.pegasus.pdl.FieldName("first"), first_schema, False, Nothing(), no_annotations), second_field := hydra.pegasus.pdl.RecordField(hydra.pegasus.pdl.FieldName("second"), second_schema, False, Nothing(), no_annotations), Right(Right(cast(hydra.pegasus.pdl.NamedSchemaType, hydra.pegasus.pdl.NamedSchemaTypeRecord(hydra.pegasus.pdl.RecordSchema((first_field, second_field), ()))))))[2]))))

            case hydra.core.TypeSet(value=st):
                return hydra.lib.eithers.bind(encode(cx, g, aliases, st), (lambda inner: Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaArray(inner))))))

            case hydra.core.TypeVariable(value=name):
                return Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaNamed(pdl_name_for_element(aliases, True, name)))))

            case hydra.core.TypeWrap(value=wt):
                cx = cx
                g = g
                aliases = aliases
                typ = wt
                continue

            case hydra.core.TypeMaybe():
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("optionals unexpected at top level"))))

            case hydra.core.TypeRecord(value=rt):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_record_field(cx, g, aliases, v1)), rt), (lambda rfields: Right(Right(cast(hydra.pegasus.pdl.NamedSchemaType, hydra.pegasus.pdl.NamedSchemaTypeRecord(hydra.pegasus.pdl.RecordSchema(rfields, ())))))))

            case hydra.core.TypeUnion(value=rt2):
                return hydra.lib.logic.if_else(hydra.lib.lists.foldl((lambda b, t: hydra.lib.logic.and_(b, hydra.lib.equality.equal(hydra.strip.deannotate_type(t), cast(hydra.core.Type, hydra.core.TypeUnit())))), True, hydra.lib.lists.map((lambda f: f.type), rt2)), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_enum_field(cx, g, v1)), rt2), (lambda fs: Right(Right(cast(hydra.pegasus.pdl.NamedSchemaType, hydra.pegasus.pdl.NamedSchemaTypeEnum(hydra.pegasus.pdl.EnumSchema(fs)))))))), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: encode_union_field(cx, g, aliases, v1)), rt2), (lambda members: Right(Left(cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaUnion(hydra.pegasus.pdl.UnionSchema(members)))))))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected ", hydra.lib.strings.cat2("PDL-supported type", hydra.lib.strings.cat2(", found: ", hydra.show.core.type(typ))))))))

def encode_union_field(cx: T0, g: hydra.graph.Graph, aliases: FrozenDict[hydra.packaging.Namespace, str], ft: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.pegasus.pdl.UnionMember]:
    name = ft.name
    typ = ft.type
    return hydra.lib.eithers.bind(get_anns(cx, g, typ), (lambda anns: hydra.lib.eithers.bind(encode_possibly_optional_type(cx, g, aliases, typ), (lambda opt_result: (s := hydra.lib.pairs.first(opt_result), optional := hydra.lib.pairs.second(opt_result), schema := hydra.lib.logic.if_else(optional, (lambda : cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaUnion(hydra.pegasus.pdl.UnionSchema(hydra.lib.lists.map((lambda ms: simple_union_member(ms)), (cast(hydra.pegasus.pdl.Schema, hydra.pegasus.pdl.SchemaNull()), s)))))), (lambda : s)), Right(hydra.pegasus.pdl.UnionMember(Just(hydra.pegasus.pdl.FieldName(name.value)), schema, anns)))[3]))))

def type_to_schema(cx: T0, g: hydra.graph.Graph, aliases: FrozenDict[hydra.packaging.Namespace, str], mod: T1, type_def: hydra.packaging.TypeDefinition) -> Either[hydra.errors.Error, tuple[hydra.pegasus.pdl.NamedSchema, frozenlist[T2]]]:
    typ = type_def.type.type
    return hydra.lib.eithers.bind(encode_type(cx, g, aliases, typ), (lambda res: (ptype := hydra.lib.eithers.either((lambda schema: cast(hydra.pegasus.pdl.NamedSchemaType, hydra.pegasus.pdl.NamedSchemaTypeTyperef(schema))), (lambda t: t), res), hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, g, typ), (lambda descr: (anns := doc(descr), qname := pdl_name_for_element(aliases, False, type_def.name), Right((hydra.pegasus.pdl.NamedSchema(qname, ptype, anns), ())))[2])))[1]))

def construct_module(cx: T0, g: hydra.graph.Graph, aliases: FrozenDict[hydra.packaging.Namespace, str], mod: hydra.packaging.Module, type_defs: frozenlist[hydra.packaging.TypeDefinition]) -> Either[hydra.errors.Error, FrozenDict[str, hydra.pegasus.pdl.SchemaFile]]:
    r"""Construct PDL schema files from type definitions, with topological sorting and cycle detection."""

    @lru_cache(1)
    def groups() -> frozenlist[frozenlist[hydra.packaging.TypeDefinition]]:
        return hydra.dependencies.topological_sort_type_definitions(type_defs)
    return hydra.lib.maybes.cases(hydra.lib.lists.find((lambda grp: hydra.lib.equality.gt(hydra.lib.lists.length(grp), 1)), groups()), (lambda : (sorted_defs := hydra.lib.lists.concat(groups()), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda type_def: type_to_schema(cx, g, aliases, mod, type_def)), sorted_defs), (lambda schemas: Right(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda v1: to_pair(mod, aliases, v1)), schemas))))))[1]), (lambda cycle: Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("types form a cycle (unsupported in PDL): [", hydra.lib.strings.cat2(hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda td: td.name.value), cycle)), "]"))))))))

def import_aliases_for_module(cx: T0, g: hydra.graph.Graph, mod: hydra.packaging.Module) -> Either[hydra.errors.Error, FrozenDict[hydra.packaging.Namespace, str]]:
    r"""Compute import aliases for a module's dependencies."""

    return hydra.lib.eithers.bind(hydra.analysis.module_dependency_namespaces(cx, g, False, True, True, False, mod), (lambda nss: Right(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ns_: (ns_, slashes_to_dots(ns_.value))), hydra.lib.sets.to_list(nss))))))

def module_to_pegasus_schemas(cx: T0, g: hydra.graph.Graph, mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition]) -> Either[hydra.errors.Error, FrozenDict[str, hydra.pegasus.pdl.SchemaFile]]:
    r"""Convert a Hydra module and its definitions to PDL schema files."""

    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.packaging.TypeDefinition], frozenlist[hydra.packaging.TermDefinition]]:
        return hydra.environment.partition_definitions(defs)
    @lru_cache(1)
    def type_defs() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.pairs.first(partitioned())
    return hydra.lib.eithers.bind(import_aliases_for_module(cx, g, mod), (lambda aliases: construct_module(cx, g, aliases, mod, type_defs())))

def module_to_pdl(mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, FrozenDict[str, str]]:
    r"""Convert a Hydra module to a map of file paths to PDL schema strings."""

    return hydra.lib.eithers.bind(module_to_pegasus_schemas(cx, g, mod, defs), (lambda files: Right(hydra.lib.maps.from_list(hydra.lib.lists.map((lambda pair: (hydra.lib.pairs.first(pair), hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.pegasus.serde.expr_schema_file(hydra.lib.pairs.second(pair)))))), hydra.lib.maps.to_list(files))))))
