# Note: this is an automatically generated file. Do not edit.

r"""GraphQL code generator: converts Hydra modules to GraphQL schema definitions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.core
import hydra.environment
import hydra.errors
import hydra.formatting
import hydra.graphql.language
import hydra.graphql.serde
import hydra.graphql.syntax
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
import hydra.predicates
import hydra.serialization
import hydra.show.core
import hydra.strip
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def description_from_type(cx: T0, g: hydra.graph.Graph, typ: hydra.core.Type) -> Either[hydra.errors.Error, Maybe[hydra.graphql.syntax.Description]]:
    return hydra.lib.eithers.map((lambda mval: hydra.lib.maybes.map((lambda s: hydra.graphql.syntax.Description(hydra.graphql.syntax.StringValue(s))), mval)), hydra.annotations.get_type_description(cx, g, typ))

def sanitize(s: str) -> str:
    return hydra.formatting.sanitize_with_underscores(hydra.graphql.language.graphql_reserved_words(), s)

def encode_enum_field_name(name: hydra.core.Name) -> hydra.graphql.syntax.EnumValue:
    return hydra.graphql.syntax.EnumValue(hydra.graphql.syntax.Name(sanitize(name.value)))

def encode_enum_field_type(cx: T0, g: hydra.graph.Graph, ft: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.graphql.syntax.EnumValueDefinition]:
    return hydra.lib.eithers.bind(description_from_type(cx, g, ft.type), (lambda desc: Right(hydra.graphql.syntax.EnumValueDefinition(desc, encode_enum_field_name(ft.name), Nothing()))))

def encode_field_name(name: hydra.core.Name) -> hydra.graphql.syntax.Name:
    return hydra.graphql.syntax.Name(sanitize(name.value))

def encode_literal_type(cx: T0, lt: hydra.core.LiteralType):
    def _hoist_hydra_graphql_coder_encode_literal_type_1(ft_, v1):
        match v1:
            case hydra.core.FloatType.FLOAT64:
                return Right(hydra.graphql.syntax.NamedType(hydra.graphql.syntax.Name("Float")))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected 64-bit float type, found: ", hydra.show.core.float_type(ft_))))))
    def _hoist_hydra_graphql_coder_encode_literal_type_2(it_, v1):
        match v1:
            case hydra.core.IntegerType.INT32:
                return Right(hydra.graphql.syntax.NamedType(hydra.graphql.syntax.Name("Int")))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected 32-bit signed integer type, found: ", hydra.show.core.integer_type(it_))))))
    match lt:
        case hydra.core.LiteralTypeBoolean():
            return Right(hydra.graphql.syntax.NamedType(hydra.graphql.syntax.Name("Boolean")))

        case hydra.core.LiteralTypeFloat(value=ft_):
            return _hoist_hydra_graphql_coder_encode_literal_type_1(ft_, ft_)

        case hydra.core.LiteralTypeInteger(value=it_):
            return _hoist_hydra_graphql_coder_encode_literal_type_2(it_, it_)

        case hydra.core.LiteralTypeString():
            return Right(hydra.graphql.syntax.NamedType(hydra.graphql.syntax.Name("String")))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected GraphQL-compatible literal type, found: ", hydra.show.core.literal_type(lt))))))

def encode_type_name(prefixes: FrozenDict[hydra.packaging.Namespace, str], name: hydra.core.Name) -> hydra.graphql.syntax.Name:
    @lru_cache(1)
    def qual_name() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    local = qual_name().local
    mns = qual_name().namespace
    @lru_cache(1)
    def prefix() -> str:
        return hydra.lib.maybes.maybe((lambda : ""), (lambda ns_: hydra.lib.maybes.maybe((lambda : ""), (lambda p: p), hydra.lib.maps.lookup(ns_, prefixes))), mns)
    return hydra.graphql.syntax.Name(hydra.lib.strings.cat2(prefix(), sanitize(local)))

def encode_type(cx: T0, g: T1, prefixes: FrozenDict[hydra.packaging.Namespace, str], typ: hydra.core.Type):
    def _hoist_hydra_graphql_coder_encode_type_1(cx, et, g, prefixes, v1):
        match v1:
            case hydra.core.TypeList(value=et2):
                return hydra.lib.eithers.map((lambda gt: cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeList(hydra.graphql.syntax.ListType(gt)))), encode_type(cx, g, prefixes, et2))

            case hydra.core.TypeSet(value=st):
                return hydra.lib.eithers.map((lambda gt: cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeList(hydra.graphql.syntax.ListType(gt)))), encode_type(cx, g, prefixes, st))

            case hydra.core.TypeMap(value=mt):
                return hydra.lib.eithers.map((lambda gt: cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeList(hydra.graphql.syntax.ListType(gt)))), encode_type(cx, g, prefixes, mt.values))

            case hydra.core.TypeLiteral(value=lt_):
                return hydra.lib.eithers.map((lambda nt: cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNamed(nt))), encode_literal_type(cx, lt_))

            case hydra.core.TypePair():
                return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNamed(hydra.graphql.syntax.NamedType(encode_type_name(prefixes, hydra.core.Name("hydra.util.Pair"))))))

            case hydra.core.TypeEither():
                return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNamed(hydra.graphql.syntax.NamedType(encode_type_name(prefixes, hydra.core.Name("hydra.util.Either"))))))

            case hydra.core.TypeRecord():
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous record type"))))

            case hydra.core.TypeUnion():
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous union type"))))

            case hydra.core.TypeWrap(value=wt):
                return encode_type(cx, g, prefixes, cast(hydra.core.Type, hydra.core.TypeMaybe(wt)))

            case hydra.core.TypeVariable(value=n):
                return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNamed(hydra.graphql.syntax.NamedType(encode_type_name(prefixes, n)))))

            case hydra.core.TypeForall(value=ft):
                return encode_type(cx, g, prefixes, cast(hydra.core.Type, hydra.core.TypeMaybe(ft.body)))

            case hydra.core.TypeApplication(value=at):
                return encode_type(cx, g, prefixes, cast(hydra.core.Type, hydra.core.TypeMaybe(at.function)))

            case hydra.core.TypeFunction():
                return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNamed(hydra.graphql.syntax.NamedType(hydra.graphql.syntax.Name("String")))))

            case hydra.core.TypeUnit():
                return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNamed(hydra.graphql.syntax.NamedType(hydra.graphql.syntax.Name("Boolean")))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected GraphQL-compatible type, found: ", hydra.show.core.type(et))))))
    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeMaybe(value=et):
            return _hoist_hydra_graphql_coder_encode_type_1(cx, et, g, prefixes, hydra.strip.deannotate_type(et))

        case hydra.core.TypeList(value=et2):
            return hydra.lib.eithers.map((lambda gt: cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNonNull(cast(hydra.graphql.syntax.NonNullType, hydra.graphql.syntax.NonNullTypeList(hydra.graphql.syntax.ListType(gt)))))), encode_type(cx, g, prefixes, et2))

        case hydra.core.TypeSet(value=st):
            return hydra.lib.eithers.map((lambda gt: cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNonNull(cast(hydra.graphql.syntax.NonNullType, hydra.graphql.syntax.NonNullTypeList(hydra.graphql.syntax.ListType(gt)))))), encode_type(cx, g, prefixes, st))

        case hydra.core.TypeMap(value=mt):
            return hydra.lib.eithers.map((lambda gt: cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNonNull(cast(hydra.graphql.syntax.NonNullType, hydra.graphql.syntax.NonNullTypeList(hydra.graphql.syntax.ListType(gt)))))), encode_type(cx, g, prefixes, mt.values))

        case hydra.core.TypeLiteral(value=lt_):
            return hydra.lib.eithers.map((lambda nt: cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNonNull(cast(hydra.graphql.syntax.NonNullType, hydra.graphql.syntax.NonNullTypeNamed(nt))))), encode_literal_type(cx, lt_))

        case hydra.core.TypePair():
            return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNonNull(cast(hydra.graphql.syntax.NonNullType, hydra.graphql.syntax.NonNullTypeNamed(hydra.graphql.syntax.NamedType(encode_type_name(prefixes, hydra.core.Name("hydra.util.Pair"))))))))

        case hydra.core.TypeEither():
            return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNonNull(cast(hydra.graphql.syntax.NonNullType, hydra.graphql.syntax.NonNullTypeNamed(hydra.graphql.syntax.NamedType(encode_type_name(prefixes, hydra.core.Name("hydra.util.Either"))))))))

        case hydra.core.TypeRecord():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous record type"))))

        case hydra.core.TypeUnion():
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("unexpected anonymous union type"))))

        case hydra.core.TypeVariable(value=n):
            return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNonNull(cast(hydra.graphql.syntax.NonNullType, hydra.graphql.syntax.NonNullTypeNamed(hydra.graphql.syntax.NamedType(encode_type_name(prefixes, n)))))))

        case hydra.core.TypeWrap(value=wt):
            return encode_type(cx, g, prefixes, wt)

        case hydra.core.TypeForall(value=ft):
            return encode_type(cx, g, prefixes, ft.body)

        case hydra.core.TypeApplication(value=at):
            return encode_type(cx, g, prefixes, at.function)

        case hydra.core.TypeFunction():
            return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNonNull(cast(hydra.graphql.syntax.NonNullType, hydra.graphql.syntax.NonNullTypeNamed(hydra.graphql.syntax.NamedType(hydra.graphql.syntax.Name("String")))))))

        case hydra.core.TypeUnit():
            return Right(cast(hydra.graphql.syntax.Type, hydra.graphql.syntax.TypeNonNull(cast(hydra.graphql.syntax.NonNullType, hydra.graphql.syntax.NonNullTypeNamed(hydra.graphql.syntax.NamedType(hydra.graphql.syntax.Name("Boolean")))))))

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected GraphQL-compatible type, found: ", hydra.show.core.type(typ))))))

def encode_field_type(cx: T0, g: hydra.graph.Graph, prefixes: FrozenDict[hydra.packaging.Namespace, str], ft: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.graphql.syntax.FieldDefinition]:
    return hydra.lib.eithers.bind(encode_type(cx, g, prefixes, ft.type), (lambda gtype: hydra.lib.eithers.bind(description_from_type(cx, g, ft.type), (lambda desc: Right(hydra.graphql.syntax.FieldDefinition(desc, encode_field_name(ft.name), Nothing(), gtype, Nothing()))))))

def encode_union_field_type(cx: T0, g: hydra.graph.Graph, prefixes: FrozenDict[hydra.packaging.Namespace, str], ft: hydra.core.FieldType) -> Either[hydra.errors.Error, hydra.graphql.syntax.FieldDefinition]:
    inner_type = ft.type
    @lru_cache(1)
    def is_unit() -> bool:
        return hydra.predicates.is_unit_type(hydra.strip.deannotate_type(inner_type))
    @lru_cache(1)
    def effective_type() -> hydra.core.Type:
        return hydra.lib.logic.if_else(is_unit(), (lambda : cast(hydra.core.Type, hydra.core.TypeMaybe(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())))))), (lambda : cast(hydra.core.Type, hydra.core.TypeMaybe(inner_type))))
    return hydra.lib.eithers.bind(encode_type(cx, g, prefixes, effective_type()), (lambda gtype: hydra.lib.eithers.bind(description_from_type(cx, g, inner_type), (lambda desc: Right(hydra.graphql.syntax.FieldDefinition(desc, encode_field_name(ft.name), Nothing(), gtype, Nothing()))))))

def encode_named_type(cx: T0, g: hydra.graph.Graph, prefixes: FrozenDict[hydra.packaging.Namespace, str], name: hydra.core.Name, typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.graphql.syntax.TypeDefinition]:
    while True:
        match hydra.strip.deannotate_type(typ):
            case hydra.core.TypeRecord(value=rt):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: encode_field_type(cx, g, prefixes, f)), rt), (lambda gfields: hydra.lib.eithers.bind(description_from_type(cx, g, typ), (lambda desc: Right(cast(hydra.graphql.syntax.TypeDefinition, hydra.graphql.syntax.TypeDefinitionObject(hydra.graphql.syntax.ObjectTypeDefinition(desc, encode_type_name(prefixes, name), Nothing(), Nothing(), Just(hydra.graphql.syntax.FieldsDefinition(gfields))))))))))

            case hydra.core.TypeUnion(value=rt2):
                return hydra.lib.logic.if_else(hydra.predicates.is_enum_row_type(rt2), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: encode_enum_field_type(cx, g, f)), rt2), (lambda values: hydra.lib.eithers.bind(description_from_type(cx, g, typ), (lambda desc: Right(cast(hydra.graphql.syntax.TypeDefinition, hydra.graphql.syntax.TypeDefinitionEnum(hydra.graphql.syntax.EnumTypeDefinition(desc, encode_type_name(prefixes, name), Nothing(), Just(hydra.graphql.syntax.EnumValuesDefinition(values))))))))))), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda f: encode_union_field_type(cx, g, prefixes, f)), rt2), (lambda gfields: hydra.lib.eithers.bind(description_from_type(cx, g, typ), (lambda desc: Right(cast(hydra.graphql.syntax.TypeDefinition, hydra.graphql.syntax.TypeDefinitionObject(hydra.graphql.syntax.ObjectTypeDefinition(desc, encode_type_name(prefixes, name), Nothing(), Nothing(), Just(hydra.graphql.syntax.FieldsDefinition(gfields))))))))))))

            case hydra.core.TypeEither(value=et):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("left"), cast(hydra.core.Type, hydra.core.TypeMaybe(et.left))), hydra.core.FieldType(hydra.core.Name("right"), cast(hydra.core.Type, hydra.core.TypeMaybe(et.right))))))
                continue

            case hydra.core.TypePair(value=pt):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("first"), pt.first), hydra.core.FieldType(hydra.core.Name("second"), pt.second))))
                continue

            case hydra.core.TypeList(value=lt_):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("value"), cast(hydra.core.Type, hydra.core.TypeList(lt_))),)))
                continue

            case hydra.core.TypeSet(value=st):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("value"), cast(hydra.core.Type, hydra.core.TypeList(st))),)))
                continue

            case hydra.core.TypeMap(value=mt):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("key"), mt.keys), hydra.core.FieldType(hydra.core.Name("value"), mt.values))))
                continue

            case hydra.core.TypeLiteral(value=lt_2):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("value"), cast(hydra.core.Type, hydra.core.TypeLiteral(lt_2))),)))
                continue

            case hydra.core.TypeVariable(value=vn):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("value"), cast(hydra.core.Type, hydra.core.TypeVariable(vn))),)))
                continue

            case hydra.core.TypeWrap(value=wt):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("value"), wt),)))
                continue

            case hydra.core.TypeUnit():
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("value"), cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean())))),)))
                continue

            case hydra.core.TypeForall(value=ft):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = ft.body
                continue

            case hydra.core.TypeApplication(value=at):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = at.function
                continue

            case hydra.core.TypeFunction(value=ft2):
                cx = cx
                g = g
                prefixes = prefixes
                name = name
                typ = cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("domain"), ft2.domain), hydra.core.FieldType(hydra.core.Name("codomain"), ft2.codomain))))
                continue

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Expected record or union type, found: ", hydra.show.core.type(typ))))))

def encode_type_definition(cx: T0, g: hydra.graph.Graph, prefixes: FrozenDict[hydra.packaging.Namespace, str], tdef: hydra.packaging.TypeDefinition) -> Either[hydra.errors.Error, hydra.graphql.syntax.TypeDefinition]:
    return encode_named_type(cx, g, prefixes, tdef.name, tdef.type.type)

def module_to_graphql(mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, FrozenDict[str, str]]:
    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.packaging.TypeDefinition], frozenlist[hydra.packaging.TermDefinition]]:
        return hydra.environment.partition_definitions(defs)
    @lru_cache(1)
    def type_defs() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.pairs.first(partitioned())
    @lru_cache(1)
    def prefixes() -> FrozenDict[hydra.packaging.Namespace, str]:
        @lru_cache(1)
        def namespaces() -> frozenlist[hydra.packaging.Namespace]:
            return hydra.lib.lists.nub(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda td: hydra.names.namespace_of(td.name)), type_defs())))
        return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda ns_: (ns_, hydra.lib.logic.if_else(hydra.lib.equality.equal(ns_, mod.namespace), (lambda : ""), (lambda : hydra.lib.strings.cat2(hydra.formatting.sanitize_with_underscores(hydra.lib.sets.empty(), ns_.value), "_"))))), namespaces()))
    @lru_cache(1)
    def file_path() -> str:
        return hydra.names.namespace_to_file_path(hydra.util.CaseConvention.CAMEL, hydra.packaging.FileExtension("graphql"), mod.namespace)
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda td: encode_type_definition(cx, g, prefixes(), td)), type_defs()), (lambda gtdefs: Right(hydra.lib.maps.from_list(hydra.lib.lists.pure((file_path(), hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.graphql.serde.expr_document(hydra.graphql.syntax.Document(hydra.lib.lists.map((lambda gtdef: cast(hydra.graphql.syntax.Definition, hydra.graphql.syntax.DefinitionTypeSystem(cast(hydra.graphql.syntax.TypeSystemDefinitionOrExtension, hydra.graphql.syntax.TypeSystemDefinitionOrExtensionDefinition(cast(hydra.graphql.syntax.TypeSystemDefinition, hydra.graphql.syntax.TypeSystemDefinitionType(gtdef))))))), gtdefs)))))))))))
