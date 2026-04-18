# Note: this is an automatically generated file. Do not edit.

r"""Protobuf code generator: converts Hydra modules to Protocol Buffers v3 definitions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt
import hydra.analysis
import hydra.annotations
import hydra.coders
import hydra.constants
import hydra.core
import hydra.decode.core
import hydra.environment
import hydra.errors
import hydra.extract.core
import hydra.formatting
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.names
import hydra.packaging
import hydra.predicates
import hydra.protobuf.environment
import hydra.protobuf.language
import hydra.protobuf.proto3
import hydra.protobuf.serde
import hydra.rewriting
import hydra.serialization
import hydra.show.core
import hydra.strip
import hydra.variables

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def simplify_type(typ: hydra.core.Type) -> hydra.core.Type:
    r"""Simplify a type by removing annotations and unwrapping newtypes."""

    while True:
        match hydra.strip.deannotate_type(typ):
            case hydra.core.TypeWrap(value=wt):
                typ = wt
                continue

            case _:
                return hydra.strip.deannotate_type(typ)

def collect_structural_types_collect_from_type(typ: hydra.core.Type):
    r"""Collect structural type references from a single type."""

    return hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda acc, t: (st := simplify_type(t), _hoist_st_body_1 := (lambda v1: (lambda et: hydra.lib.sets.insert(cast(hydra.protobuf.environment.StructuralTypeRef, hydra.protobuf.environment.StructuralTypeRefEither((et.left, et.right))), acc))(v1.value) if isinstance(v1, hydra.core.TypeEither) else (lambda pt: hydra.lib.sets.insert(cast(hydra.protobuf.environment.StructuralTypeRef, hydra.protobuf.environment.StructuralTypeRefPair((pt.first, pt.second))), acc))(v1.value) if isinstance(v1, hydra.core.TypePair) else acc), _hoist_st_body_1(st))[2]), hydra.lib.sets.empty(), typ)

def collect_structural_types(types: frozenlist[hydra.core.Type]) -> frozenset[hydra.protobuf.environment.StructuralTypeRef]:
    r"""Collect all structural type references (Either, Pair) from a list of types."""

    return hydra.lib.lists.foldl((lambda acc, t: hydra.lib.sets.union(acc, collect_structural_types_collect_from_type(t))), hydra.lib.sets.empty(), types)

def encode_enum_value_name(tname: hydra.core.Name, fname: hydra.core.Name) -> hydra.protobuf.proto3.EnumValueName:
    r"""Encode an enum value name from type name and field name."""

    @lru_cache(1)
    def prefix() -> str:
        return hydra.formatting.non_alnum_to_underscores(hydra.formatting.convert_case_camel_to_upper_snake(hydra.names.local_name_of(tname)))
    @lru_cache(1)
    def suffix() -> str:
        return hydra.formatting.non_alnum_to_underscores(hydra.formatting.convert_case_camel_to_upper_snake(fname.value))
    return hydra.protobuf.proto3.EnumValueName(hydra.lib.strings.cat((prefix(), "_", suffix())))

def encode_type_name(name: hydra.core.Name) -> hydra.protobuf.proto3.TypeName:
    r"""Encode a Hydra type name as a Protobuf type name."""

    return hydra.protobuf.proto3.TypeName(hydra.names.local_name_of(name))

def read_boolean_annotation(cx: T0, g: hydra.graph.Graph, key: hydra.core.Name, typ: hydra.core.Type) -> Either[hydra.errors.Error, bool]:
    r"""Read a boolean annotation from a type."""

    return hydra.lib.maybes.maybe((lambda : Right(False)), (lambda term: hydra.extract.core.boolean(g, term)), hydra.lib.maps.lookup(key, hydra.annotations.type_annotation_internal(typ)))

def find_options(cx: T0, g: hydra.graph.Graph, typ: hydra.core.Type) -> Either[hydra.errors.Error, frozenlist[hydra.protobuf.proto3.Option]]:
    r"""Find Protobuf options for a type (description and deprecated)."""

    return hydra.lib.eithers.bind(hydra.annotations.get_type_description(cx, g, typ), (lambda mdesc: hydra.lib.eithers.bind(read_boolean_annotation(cx, g, hydra.constants.key_deprecated, typ), (lambda bdep: (mdesc_ann := hydra.lib.maybes.map((lambda desc_: hydra.protobuf.proto3.Option(hydra.protobuf.serde.description_option_name, cast(hydra.protobuf.proto3.Value, hydra.protobuf.proto3.ValueString(desc_)))), mdesc), mdep_ann := hydra.lib.logic.if_else(bdep, (lambda : Just(hydra.protobuf.proto3.Option("deprecated", cast(hydra.protobuf.proto3.Value, hydra.protobuf.proto3.ValueBoolean(True))))), (lambda : Nothing())), Right(hydra.lib.maybes.cat((mdesc_ann, mdep_ann))))[2]))))

def encode_enum_definition(cx: T0, g: hydra.graph.Graph, options: frozenlist[hydra.protobuf.proto3.Option], tname: hydra.core.Name, fts: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, hydra.protobuf.proto3.EnumDefinition]:
    r"""Encode a Hydra union type as a Protobuf enum definition."""

    @lru_cache(1)
    def unspecified_field() -> hydra.protobuf.proto3.EnumValue:
        return hydra.protobuf.proto3.EnumValue(encode_enum_value_name(tname, hydra.core.Name("unspecified")), 0, ())
    def encode_enum_field(field: hydra.core.FieldType, idx: int) -> Either[hydra.errors.Error, hydra.protobuf.proto3.EnumValue]:
        fname = field.name
        ftype = field.type
        return hydra.lib.eithers.bind(find_options(cx, g, ftype), (lambda opts: Right(hydra.protobuf.proto3.EnumValue(encode_enum_value_name(tname, fname), idx, opts))))
    @lru_cache(1)
    def indices() -> frozenlist[int]:
        return hydra.lib.math.range_(1, hydra.lib.lists.length(fts))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda p: encode_enum_field(hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), hydra.lib.lists.zip(fts, indices())), (lambda values: Right(hydra.protobuf.proto3.EnumDefinition(encode_type_name(tname), hydra.lib.lists.cons(unspecified_field(), values), options))))

def encode_field_name(preserve: bool, name: hydra.core.Name) -> hydra.protobuf.proto3.FieldName:
    r"""Encode a field name, optionally preserving the original case."""

    return hydra.protobuf.proto3.FieldName(hydra.lib.logic.if_else(preserve, (lambda : name.value), (lambda : hydra.formatting.convert_case_camel_to_lower_snake(name.value))))

def err(cx: T0, msg: str) -> Either[hydra.errors.Error, T1]:
    return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(msg))))

def unexpected_e(cx: T0, expected: str, found: str) -> Either[hydra.errors.Error, T1]:
    return err(cx, hydra.lib.strings.cat(("Expected ", expected, ", found: ", found)))

def encode_scalar_type(cx: T0, lt: hydra.core.LiteralType):
    def _hoist_hydra_protobuf_coder_encode_scalar_type_1(cx, ft, v1):
        match v1:
            case hydra.core.FloatType.FLOAT32:
                return Right(hydra.protobuf.proto3.ScalarType.FLOAT)

            case hydra.core.FloatType.FLOAT64:
                return Right(hydra.protobuf.proto3.ScalarType.DOUBLE)

            case _:
                return unexpected_e(cx, "32-bit or 64-bit floating-point type", hydra.show.core.float_type(ft))
    def _hoist_hydra_protobuf_coder_encode_scalar_type_2(cx, it, v1):
        match v1:
            case hydra.core.IntegerType.INT32:
                return Right(hydra.protobuf.proto3.ScalarType.INT32)

            case hydra.core.IntegerType.INT64:
                return Right(hydra.protobuf.proto3.ScalarType.INT64)

            case hydra.core.IntegerType.UINT32:
                return Right(hydra.protobuf.proto3.ScalarType.UINT32)

            case hydra.core.IntegerType.UINT64:
                return Right(hydra.protobuf.proto3.ScalarType.UINT64)

            case _:
                return unexpected_e(cx, "32-bit or 64-bit integer type", hydra.show.core.integer_type(it))
    match lt:
        case hydra.core.LiteralTypeBinary():
            return Right(hydra.protobuf.proto3.ScalarType.BYTES)

        case hydra.core.LiteralTypeBoolean():
            return Right(hydra.protobuf.proto3.ScalarType.BOOL)

        case hydra.core.LiteralTypeFloat(value=ft):
            return _hoist_hydra_protobuf_coder_encode_scalar_type_1(cx, ft, ft)

        case hydra.core.LiteralTypeInteger(value=it):
            return _hoist_hydra_protobuf_coder_encode_scalar_type_2(cx, it, it)

        case hydra.core.LiteralTypeString():
            return Right(hydra.protobuf.proto3.ScalarType.STRING)

        case _:
            return unexpected_e(cx, "supported literal type", hydra.show.core.literal_type(lt))

def encode_scalar_type_wrapped(cx: T0, lt: hydra.core.LiteralType):
    r"""Encode a Hydra literal type as a wrapped Protobuf type (for optional scalars)."""

    def to_type(label: str) -> Either[T1, hydra.protobuf.proto3.SimpleType]:
        return Right(cast(hydra.protobuf.proto3.SimpleType, hydra.protobuf.proto3.SimpleTypeReference(hydra.protobuf.proto3.TypeName(hydra.lib.strings.cat(("google.protobuf.", label, "Value"))))))
    def _hoist_to_type_body_1(ft, v1):
        match v1:
            case hydra.core.FloatType.FLOAT32:
                return to_type("Float")

            case hydra.core.FloatType.FLOAT64:
                return to_type("Double")

            case _:
                return unexpected_e(cx, "32-bit or 64-bit floating-point type", hydra.show.core.float_type(ft))
    def _hoist_to_type_body_2(it, v1):
        match v1:
            case hydra.core.IntegerType.INT32:
                return to_type("Int32")

            case hydra.core.IntegerType.INT64:
                return to_type("Int64")

            case hydra.core.IntegerType.UINT32:
                return to_type("UInt32")

            case hydra.core.IntegerType.UINT64:
                return to_type("UInt64")

            case _:
                return unexpected_e(cx, "32-bit or 64-bit integer type", hydra.show.core.integer_type(it))
    match lt:
        case hydra.core.LiteralTypeBinary():
            return to_type("Bytes")

        case hydra.core.LiteralTypeBoolean():
            return to_type("Bool")

        case hydra.core.LiteralTypeFloat(value=ft):
            return _hoist_to_type_body_1(ft, ft)

        case hydra.core.LiteralTypeInteger(value=it):
            return _hoist_to_type_body_2(it, it)

        case hydra.core.LiteralTypeString():
            return to_type("String")

        case _:
            return unexpected_e(cx, "supported literal type", hydra.show.core.literal_type(lt))

def encode_type_reference(local_ns: hydra.packaging.Namespace, name: hydra.core.Name) -> hydra.protobuf.proto3.TypeName:
    r"""Encode a Hydra name as a Protobuf type reference."""

    @lru_cache(1)
    def qn() -> hydra.packaging.QualifiedName:
        return hydra.names.qualify_name(name)
    local = qn().local
    ns_ = qn().namespace
    @lru_cache(1)
    def local_ns_parts() -> frozenlist[str]:
        return hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.lists.maybe_init(hydra.lib.strings.split_on(".", local_ns.value)))
    return hydra.protobuf.proto3.TypeName(hydra.lib.maybes.maybe((lambda : local), (lambda ns_val: (ns_parts := hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.lists.maybe_init(hydra.lib.strings.split_on(".", ns_val.value))), hydra.lib.logic.if_else(hydra.lib.equality.equal(ns_parts, local_ns_parts()), (lambda : local), (lambda : hydra.lib.strings.intercalate(".", hydra.lib.lists.concat((ns_parts, (local,)))))))[1]), ns_))

key_proto_field_index = hydra.core.Name("proto_field_index")

def map_accum_result(f: Callable[[T0, T1], Either[T2, tuple[T3, T0]]], cx0: T0, xs: frozenlist[T1]) -> Either[T2, tuple[frozenlist[T3], T0]]:
    r"""Thread context through a list, accumulating results."""

    return hydra.lib.lists.foldl((lambda acc_e, x: hydra.lib.eithers.bind(acc_e, (lambda acc_pair: (bs := hydra.lib.pairs.first(acc_pair), cx_n := hydra.lib.pairs.second(acc_pair), hydra.lib.eithers.map((lambda result_pair: (hydra.lib.lists.concat((bs, (hydra.lib.pairs.first(result_pair),))), hydra.lib.pairs.second(result_pair))), f(cx_n, x)))[2]))), Right(((), cx0)), xs)

def structural_type_name(local_ns: T0, ref: hydra.protobuf.environment.StructuralTypeRef):
    r"""Generate a message name for a structural type reference."""

    def type_suffix(typ: hydra.core.Type):
        @lru_cache(1)
        def st() -> hydra.core.Type:
            return simplify_type(typ)
        def _hoist_st_body_1(v1):
            match v1:
                case hydra.core.FloatType.FLOAT32:
                    return "float"

                case hydra.core.FloatType.FLOAT64:
                    return "double"

                case _:
                    return "float"
        def _hoist_st_body_2(v1):
            match v1:
                case hydra.core.IntegerType.INT32:
                    return "int32"

                case hydra.core.IntegerType.INT64:
                    return "int64"

                case hydra.core.IntegerType.UINT32:
                    return "uint32"

                case hydra.core.IntegerType.UINT64:
                    return "uint64"

                case _:
                    return "int64"
        def _hoist_st_body_3(v1):
            match v1:
                case hydra.core.LiteralTypeBinary():
                    return "bytes"

                case hydra.core.LiteralTypeBoolean():
                    return "bool"

                case hydra.core.LiteralTypeFloat(value=ft):
                    return _hoist_st_body_1(ft)

                case hydra.core.LiteralTypeInteger(value=it):
                    return _hoist_st_body_2(it)

                case hydra.core.LiteralTypeString():
                    return "string"

                case _:
                    return "value"
        match st():
            case hydra.core.TypeLiteral(value=lt):
                return _hoist_st_body_3(lt)

            case hydra.core.TypeRecord():
                return "record"

            case hydra.core.TypeUnion():
                return "union"

            case hydra.core.TypeVariable(value=name):
                return hydra.names.local_name_of(name)

            case hydra.core.TypeUnit():
                return "unit"

            case hydra.core.TypeList():
                return "list"

            case hydra.core.TypeSet():
                return "set"

            case hydra.core.TypeMap():
                return "map"

            case hydra.core.TypeMaybe():
                return "maybe"

            case _:
                return "value"
    def _hoist_type_suffix_body_1(v1):
        match v1:
            case hydra.protobuf.environment.StructuralTypeRefEither(value=p):
                return hydra.lib.strings.cat(("Either_", type_suffix(hydra.lib.pairs.first(p)), "_", type_suffix(hydra.lib.pairs.second(p))))

            case hydra.protobuf.environment.StructuralTypeRefPair(value=p):
                return hydra.lib.strings.cat(("Pair_", type_suffix(hydra.lib.pairs.first(p)), "_", type_suffix(hydra.lib.pairs.second(p))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.protobuf.proto3.TypeName(_hoist_type_suffix_body_1(ref))

def encode_field_type(cx: hydra.context.Context, g: hydra.graph.Graph, local_ns: hydra.packaging.Namespace, ft: hydra.core.FieldType) -> Either[hydra.errors.Error, tuple[hydra.protobuf.proto3.Field, hydra.context.Context]]:
    r"""Encode a Hydra field type as a Protobuf field."""

    fname = ft.name
    ftype = ft.type
    def encode_type_(cx0: hydra.context.Context, g0: hydra.graph.Graph, ns0: hydra.packaging.Namespace, typ: hydra.core.Type):
        def _hoist_encode_type_1(cx0, g0, ns0, ot, v1):
            match v1:
                case hydra.core.TypeLiteral(value=lt):
                    return hydra.lib.eithers.map((lambda st: cast(hydra.protobuf.proto3.FieldType, hydra.protobuf.proto3.FieldTypeSimple(st))), encode_scalar_type_wrapped(cx0, lt))

                case _:
                    return encode_type_(cx0, g0, ns0, ot)
        match simplify_type(typ):
            case hydra.core.TypeEither(value=et):
                @lru_cache(1)
                def ref() -> hydra.protobuf.environment.StructuralTypeRef:
                    return cast(hydra.protobuf.environment.StructuralTypeRef, hydra.protobuf.environment.StructuralTypeRefEither((et.left, et.right)))
                return Right(cast(hydra.protobuf.proto3.FieldType, hydra.protobuf.proto3.FieldTypeSimple(cast(hydra.protobuf.proto3.SimpleType, hydra.protobuf.proto3.SimpleTypeReference(structural_type_name(ns0, ref()))))))

            case hydra.core.TypePair(value=pt):
                @lru_cache(1)
                def ref() -> hydra.protobuf.environment.StructuralTypeRef:
                    return cast(hydra.protobuf.environment.StructuralTypeRef, hydra.protobuf.environment.StructuralTypeRefPair((pt.first, pt.second)))
                return Right(cast(hydra.protobuf.proto3.FieldType, hydra.protobuf.proto3.FieldTypeSimple(cast(hydra.protobuf.proto3.SimpleType, hydra.protobuf.proto3.SimpleTypeReference(structural_type_name(ns0, ref()))))))

            case hydra.core.TypeList(value=lt):
                return hydra.lib.eithers.map((lambda st: cast(hydra.protobuf.proto3.FieldType, hydra.protobuf.proto3.FieldTypeRepeated(st))), encode_simple_type_(cx0, g0, ns0, True, lt))

            case hydra.core.TypeSet(value=set_type):
                return hydra.lib.eithers.map((lambda st: cast(hydra.protobuf.proto3.FieldType, hydra.protobuf.proto3.FieldTypeRepeated(st))), encode_simple_type_(cx0, g0, ns0, True, set_type))

            case hydra.core.TypeMap(value=mt):
                return hydra.lib.eithers.bind(encode_simple_type_(cx0, g0, ns0, False, mt.keys), (lambda kt: hydra.lib.eithers.bind(encode_simple_type_(cx0, g0, ns0, True, mt.values), (lambda vt: Right(cast(hydra.protobuf.proto3.FieldType, hydra.protobuf.proto3.FieldTypeMap(hydra.protobuf.proto3.MapType(kt, vt))))))))

            case hydra.core.TypeMaybe(value=ot):
                return _hoist_encode_type_1(cx0, g0, ns0, ot, hydra.strip.deannotate_type(ot))

            case hydra.core.TypeUnion(value=fts):
                return hydra.lib.eithers.bind(map_accum_result((lambda cx_, f: encode_field_type(cx_, g0, ns0, f)), cx0, fts), (lambda pfields: (fields_ := hydra.lib.pairs.first(pfields), Right(cast(hydra.protobuf.proto3.FieldType, hydra.protobuf.proto3.FieldTypeOneof(fields_))))[1]))

            case _:
                return hydra.lib.eithers.map((lambda st: cast(hydra.protobuf.proto3.FieldType, hydra.protobuf.proto3.FieldTypeSimple(st))), encode_simple_type_(cx0, g0, ns0, True, typ))
    def encode_simple_type_(cx0: T0, g0: hydra.graph.Graph, ns0: hydra.packaging.Namespace, noms: bool, typ: hydra.core.Type):
        def for_nominal(name: hydra.core.Name) -> Either[T1, hydra.protobuf.proto3.SimpleType]:
            return Right(cast(hydra.protobuf.proto3.SimpleType, hydra.protobuf.proto3.SimpleTypeReference(encode_type_reference(ns0, name))))
        def _hoist_for_nominal_body_1(v1):
            match v1:
                case hydra.core.TypeLiteral(value=lt):
                    return hydra.lib.eithers.map((lambda st: cast(hydra.protobuf.proto3.SimpleType, hydra.protobuf.proto3.SimpleTypeScalar(st))), encode_scalar_type(cx0, lt))

                case hydra.core.TypeRecord():
                    return unexpected_e(cx0, "named type reference", "anonymous record type")

                case hydra.core.TypeUnion():
                    return unexpected_e(cx0, "named type reference", "anonymous union type")

                case hydra.core.TypeUnit():
                    return Right(cast(hydra.protobuf.proto3.SimpleType, hydra.protobuf.proto3.SimpleTypeReference(hydra.protobuf.proto3.TypeName("google.protobuf.Empty"))))

                case hydra.core.TypeVariable(value=name):
                    return hydra.lib.logic.if_else(noms, (lambda : for_nominal(name)), (lambda : hydra.lib.eithers.bind(hydra.lexical.require_binding(g0, name), (lambda el: (term := el.term, hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda de: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(de.value)))), (lambda t: t), hydra.decode.core.type(g0, term)), (lambda resolved_typ: encode_simple_type_(cx0, g0, ns0, noms, resolved_typ))))[1]))))

                case _:
                    return unexpected_e(cx0, "simple type", hydra.show.core.type(hydra.strip.remove_type_annotations(typ)))
        return _hoist_for_nominal_body_1(simplify_type(typ))
    return hydra.lib.eithers.bind(find_options(cx, g, ftype), (lambda options: hydra.lib.eithers.bind(encode_type_(cx, g, local_ns, ftype), (lambda ft_: (idx_pair := hydra.annotations.next_count(key_proto_field_index, cx), idx := hydra.lib.pairs.first(idx_pair), cx1 := hydra.lib.pairs.second(idx_pair), hydra.lib.eithers.bind(read_boolean_annotation(cx, g, hydra.constants.key_preserve_field_name, ftype), (lambda preserve: Right((hydra.protobuf.proto3.Field(encode_field_name(preserve, fname), Nothing(), ft_, idx, options), cx1)))))[3]))))

def encode_record_type(cx: hydra.context.Context, g: hydra.graph.Graph, local_ns: hydra.packaging.Namespace, options: frozenlist[hydra.protobuf.proto3.Option], tname: hydra.core.Name, fts: frozenlist[hydra.core.FieldType]) -> Either[hydra.errors.Error, hydra.protobuf.proto3.MessageDefinition]:
    r"""Encode a Hydra record type as a Protobuf message definition."""

    return hydra.lib.eithers.bind(map_accum_result((lambda cx_, f: encode_field_type(cx_, g, local_ns, f)), cx, fts), (lambda result: (pfields := hydra.lib.pairs.first(result), Right(hydra.protobuf.proto3.MessageDefinition(encode_type_name(tname), pfields, options)))[1]))

def is_enum_fields(fts: frozenlist[hydra.core.FieldType]) -> bool:
    r"""Check if all fields are unit types (i.e., this is an enum)."""

    return hydra.lib.lists.foldl((lambda b, f: hydra.lib.logic.and_(b, hydra.predicates.is_unit_type(simplify_type(f.type)))), True, fts)

def is_enum_definition(typ: hydra.core.Type) -> bool:
    r"""Check if a type is an enum definition."""

    match simplify_type(typ):
        case hydra.core.TypeUnion(value=fts):
            return is_enum_fields(fts)

        case _:
            return False

def encode_definition(cx: hydra.context.Context, g: hydra.graph.Graph, local_ns: hydra.packaging.Namespace, name: hydra.core.Name, typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.protobuf.proto3.Definition]:
    r"""Encode a Hydra type as a Protobuf definition."""

    @lru_cache(1)
    def cx1() -> hydra.context.Context:
        return hydra.annotations.reset_count(key_proto_field_index, cx)
    @lru_cache(1)
    def cx2() -> hydra.context.Context:
        return hydra.lib.pairs.second(hydra.annotations.next_count(key_proto_field_index, cx1()))
    def wrap_as_record_type(t: hydra.core.Type) -> hydra.core.Type:
        return cast(hydra.core.Type, hydra.core.TypeRecord((hydra.core.FieldType(hydra.core.Name("value"), t),)))
    def to_either_string(result: T0) -> T0:
        return result
    def encode(cx0: hydra.context.Context, options: frozenlist[hydra.protobuf.proto3.Option], t: hydra.core.Type) -> Either[hydra.errors.Error, hydra.protobuf.proto3.Definition]:
        match simplify_type(t):
            case hydra.core.TypeRecord(value=fts):
                return hydra.lib.eithers.map((lambda md: cast(hydra.protobuf.proto3.Definition, hydra.protobuf.proto3.DefinitionMessage(md))), to_either_string(encode_record_type(cx0, g, local_ns, options, name, fts)))

            case hydra.core.TypeUnion(value=fts2):
                return hydra.lib.logic.if_else(is_enum_definition(t), (lambda : hydra.lib.eithers.map((lambda ed: cast(hydra.protobuf.proto3.Definition, hydra.protobuf.proto3.DefinitionEnum(ed))), to_either_string(encode_enum_definition(cx0, g, options, name, fts2)))), (lambda : encode(cx0, options, wrap_as_record_type(cast(hydra.core.Type, hydra.core.TypeUnion(fts2))))))

            case _:
                return encode(cx0, options, wrap_as_record_type(t))
    return hydra.lib.eithers.bind(to_either_string(find_options(cx, g, typ)), (lambda options: encode(cx2(), options, typ)))

def flatten_type(typ: hydra.core.Type):
    def _hoist_hydra_protobuf_coder_flatten_type_1(recurse, t, v1):
        match v1:
            case hydra.core.TypeForall(value=ft):
                return recurse(hydra.variables.replace_free_type_variable(ft.parameter, cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))), ft.body))

            case hydra.core.TypeApplication(value=at):
                return recurse(at.function)

            case _:
                return recurse(t)
    return hydra.rewriting.rewrite_type((lambda recurse, t: _hoist_hydra_protobuf_coder_flatten_type_1(recurse, t, t)), typ)

def encode_simple_type_for_helper(cx: T0, local_ns: hydra.packaging.Namespace, typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.protobuf.proto3.SimpleType]:
    r"""Encode a simple type for helper message fields."""

    def for_nominal(name: hydra.core.Name) -> Either[T1, hydra.protobuf.proto3.SimpleType]:
        return Right(cast(hydra.protobuf.proto3.SimpleType, hydra.protobuf.proto3.SimpleTypeReference(encode_type_reference(local_ns, name))))
    match simplify_type(typ):
        case hydra.core.TypeLiteral(value=lt):
            return hydra.lib.eithers.map((lambda st: cast(hydra.protobuf.proto3.SimpleType, hydra.protobuf.proto3.SimpleTypeScalar(st))), encode_scalar_type(cx, lt))

        case hydra.core.TypeRecord():
            return unexpected_e(cx, "named type reference", "anonymous record type")

        case hydra.core.TypeUnion():
            return unexpected_e(cx, "named type reference", "anonymous union type")

        case hydra.core.TypeUnit():
            return Right(cast(hydra.protobuf.proto3.SimpleType, hydra.protobuf.proto3.SimpleTypeReference(hydra.protobuf.proto3.TypeName("google.protobuf.Empty"))))

        case hydra.core.TypeVariable(value=name):
            return for_nominal(name)

        case _:
            return unexpected_e(cx, "simple type in structural type helper", hydra.show.core.type(hydra.strip.remove_type_annotations(typ)))

def generate_structural_type_message(cx: hydra.context.Context, g: T0, local_ns: hydra.packaging.Namespace, ref: hydra.protobuf.environment.StructuralTypeRef) -> Either[hydra.errors.Error, tuple[hydra.protobuf.proto3.Definition, hydra.context.Context]]:
    r"""Generate a helper message definition for a structural type."""

    @lru_cache(1)
    def cx1() -> hydra.context.Context:
        return hydra.annotations.reset_count(key_proto_field_index, cx)
    @lru_cache(1)
    def cx2() -> hydra.context.Context:
        return hydra.lib.pairs.second(hydra.annotations.next_count(key_proto_field_index, cx1()))
    def make_field(cx0: hydra.context.Context, fname: str, ftyp: hydra.core.Type) -> Either[hydra.errors.Error, tuple[hydra.protobuf.proto3.Field, hydra.context.Context]]:
        return hydra.lib.eithers.bind(encode_simple_type_for_helper(cx0, local_ns, ftyp), (lambda ft: (idx_pair := hydra.annotations.next_count(key_proto_field_index, cx0), idx := hydra.lib.pairs.first(idx_pair), cx1_ := hydra.lib.pairs.second(idx_pair), Right((hydra.protobuf.proto3.Field(hydra.protobuf.proto3.FieldName(fname), Nothing(), cast(hydra.protobuf.proto3.FieldType, hydra.protobuf.proto3.FieldTypeSimple(ft)), idx, ()), cx1_)))[3]))
    match ref:
        case hydra.protobuf.environment.StructuralTypeRefEither(value=p):
            @lru_cache(1)
            def lt() -> hydra.core.Type:
                return hydra.lib.pairs.first(p)
            @lru_cache(1)
            def rt() -> hydra.core.Type:
                return hydra.lib.pairs.second(p)
            return hydra.lib.eithers.bind(make_field(cx2(), "left", lt()), (lambda left_result: (left_field := hydra.lib.pairs.first(left_result), cx3 := hydra.lib.pairs.second(left_result), hydra.lib.eithers.bind(make_field(cx3, "right", rt()), (lambda right_result: (right_field := hydra.lib.pairs.first(right_result), cx4 := hydra.lib.pairs.second(right_result), Right((cast(hydra.protobuf.proto3.Definition, hydra.protobuf.proto3.DefinitionMessage(hydra.protobuf.proto3.MessageDefinition(structural_type_name(local_ns, ref), (left_field, right_field), ()))), cx4)))[2])))[2]))

        case hydra.protobuf.environment.StructuralTypeRefPair(value=p2):
            @lru_cache(1)
            def ft() -> hydra.core.Type:
                return hydra.lib.pairs.first(p2)
            @lru_cache(1)
            def st() -> hydra.core.Type:
                return hydra.lib.pairs.second(p2)
            return hydra.lib.eithers.bind(make_field(cx2(), "first", ft()), (lambda first_result: (first_field := hydra.lib.pairs.first(first_result), cx3 := hydra.lib.pairs.second(first_result), hydra.lib.eithers.bind(make_field(cx3, "second", st()), (lambda second_result: (second_field := hydra.lib.pairs.first(second_result), cx4 := hydra.lib.pairs.second(second_result), Right((cast(hydra.protobuf.proto3.Definition, hydra.protobuf.proto3.DefinitionMessage(hydra.protobuf.proto3.MessageDefinition(structural_type_name(local_ns, ref), (first_field, second_field), ()))), cx4)))[2])))[2]))

        case _:
            raise AssertionError("Unreachable: all variants handled")

java_multiple_files_option_name = "java_multiple_files"

java_package_option_name = "java_package"

def namespace_to_file_reference(ns_: hydra.packaging.Namespace) -> hydra.protobuf.proto3.FileReference:
    r"""Convert a Hydra namespace to a Protobuf file reference."""

    @lru_cache(1)
    def pns() -> str:
        return hydra.lib.strings.intercalate("/", hydra.lib.lists.map((lambda s: hydra.formatting.convert_case_camel_to_lower_snake(s)), hydra.lib.strings.split_on(".", ns_.value)))
    return hydra.protobuf.proto3.FileReference(hydra.lib.strings.cat2(pns(), ".proto"))

def namespace_to_package_name(ns_: hydra.packaging.Namespace) -> hydra.protobuf.proto3.PackageName:
    r"""Convert a Hydra namespace to a Protobuf package name."""

    return hydra.protobuf.proto3.PackageName(hydra.lib.strings.intercalate(".", hydra.lib.lists.map((lambda s: hydra.formatting.convert_case_camel_to_lower_snake(s)), hydra.lib.maybes.from_maybe((lambda : ()), hydra.lib.lists.maybe_init(hydra.lib.strings.split_on(".", ns_.value))))))

def construct_module(cx: hydra.context.Context, g: hydra.graph.Graph, mod: hydra.packaging.Module, type_defs: frozenlist[hydra.packaging.TypeDefinition]) -> Either[hydra.errors.Error, hydra.protobuf.proto3.ProtoFile]:
    r"""Construct a Protobuf file from a Hydra module and its type definitions."""

    ns_ = mod.namespace
    desc = mod.description
    def to_def(td: hydra.packaging.TypeDefinition) -> Either[hydra.errors.Error, hydra.protobuf.proto3.Definition]:
        name = td.name
        typ = td.type.type
        def encode_def_either(n: hydra.core.Name, t: hydra.core.Type) -> Either[hydra.errors.Error, hydra.protobuf.proto3.Definition]:
            return encode_definition(cx, g, ns_, n, t)
        @lru_cache(1)
        def flat_typ() -> hydra.core.Type:
            return flatten_type(typ)
        def enc(v1: hydra.core.Type) -> Either[hydra.errors.Error, hydra.protobuf.proto3.Definition]:
            return encode_def_either(name, v1)
        match hydra.strip.deannotate_type(flat_typ()):
            case hydra.core.TypeVariable():
                return enc(flat_typ())

            case _:
                return hydra.lib.eithers.bind(hydra.adapt.adapt_type_for_language(hydra.protobuf.language.protobuf_language(), flat_typ()), (lambda adapted_type: enc(adapted_type)))
    @lru_cache(1)
    def types() -> frozenlist[hydra.core.Type]:
        return hydra.lib.lists.map((lambda td: td.type.type), type_defs)
    @lru_cache(1)
    def struct_refs() -> frozenset[hydra.protobuf.environment.StructuralTypeRef]:
        return collect_structural_types(types())
    @lru_cache(1)
    def java_options() -> frozenlist[hydra.protobuf.proto3.Option]:
        return (hydra.protobuf.proto3.Option(java_multiple_files_option_name, cast(hydra.protobuf.proto3.Value, hydra.protobuf.proto3.ValueBoolean(True))), hydra.protobuf.proto3.Option(java_package_option_name, cast(hydra.protobuf.proto3.Value, hydra.protobuf.proto3.ValueString(namespace_to_package_name(ns_).value))))
    @lru_cache(1)
    def desc_option() -> hydra.protobuf.proto3.Option:
        return hydra.protobuf.proto3.Option(hydra.protobuf.serde.description_option_name, cast(hydra.protobuf.proto3.Value, hydra.protobuf.proto3.ValueString(hydra.lib.strings.cat2(hydra.lib.maybes.maybe((lambda : ""), (lambda d: hydra.lib.strings.cat2(d, "\n\n")), desc), hydra.constants.warning_auto_generated_file))))
    def check_field_type_wrapper(typ: hydra.core.Type):
        def _hoist_check_field_type_wrapper_1(v1):
            match v1:
                case hydra.core.TypeLiteral():
                    return True

                case _:
                    return False
        match hydra.strip.deannotate_type(typ):
            case hydra.core.TypeMaybe(value=ot):
                return _hoist_check_field_type_wrapper_1(hydra.strip.deannotate_type(ot))

            case _:
                return False
    def check_field_type_empty(typ: hydra.core.Type) -> bool:
        return hydra.predicates.is_unit_type(typ)
    def check_fields(check_type: Callable[[hydra.core.Type], Maybe[bool]], check_field_type: Callable[[hydra.core.Type], bool], ts: frozenlist[hydra.core.Type]):
        return hydra.lib.lists.foldl((lambda b, t: hydra.lib.logic.or_(b, hydra.rewriting.fold_over_type(hydra.coders.TraversalOrder.PRE, (lambda b2, t2: hydra.lib.logic.or_(b2, (check_result := check_type(t2), (_hoist_check_result_body_1 := (lambda v1: (lambda fts: hydra.lib.lists.foldl((lambda b3, f: hydra.lib.logic.or_(b3, check_field_type(hydra.strip.deannotate_type(f.type)))), False, fts))(v1.value) if isinstance(v1, hydra.core.TypeRecord) else (lambda fts: hydra.lib.lists.foldl((lambda b3, f: hydra.lib.logic.or_(b3, check_field_type(hydra.strip.deannotate_type(f.type)))), False, fts))(v1.value) if isinstance(v1, hydra.core.TypeUnion) else False), hydra.lib.maybes.maybe((lambda : _hoist_check_result_body_1(t2)), (lambda b3: b3), check_result))[1])[1])), False, t))), False, ts)
    @lru_cache(1)
    def wrapper_import() -> frozenlist[hydra.protobuf.proto3.FileReference]:
        return hydra.lib.logic.if_else(check_fields((lambda _: Nothing()), (lambda x1: check_field_type_wrapper(x1)), types()), (lambda : (hydra.protobuf.proto3.FileReference("google/protobuf/wrappers.proto"),)), (lambda : ()))
    def empty_check_type(typ: hydra.core.Type) -> Maybe[bool]:
        return hydra.lib.logic.if_else(is_enum_definition(typ), (lambda : Just(False)), (lambda : Nothing()))
    @lru_cache(1)
    def empty_import() -> frozenlist[hydra.protobuf.proto3.FileReference]:
        return hydra.lib.logic.if_else(check_fields((lambda x1: empty_check_type(x1)), (lambda x1: check_field_type_empty(x1)), types()), (lambda : (hydra.protobuf.proto3.FileReference("google/protobuf/empty.proto"),)), (lambda : ()))
    return hydra.lib.eithers.bind(hydra.analysis.module_dependency_namespaces(cx, g, True, False, False, False, mod), (lambda schema_imports: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: to_def(x1)), type_defs), (lambda definitions: (schema_import_list := hydra.lib.lists.map((lambda n: namespace_to_file_reference(n)), hydra.lib.sets.to_list(schema_imports)), hydra.lib.eithers.bind(map_accum_result((lambda cx0, ref: generate_structural_type_message(cx0, g, ns_, ref)), cx, hydra.lib.sets.to_list(struct_refs())), (lambda helper_result: (helper_defs := hydra.lib.pairs.first(helper_result), Right(hydra.protobuf.proto3.ProtoFile(namespace_to_package_name(ns_), hydra.lib.lists.concat((schema_import_list, wrapper_import(), empty_import())), hydra.lib.lists.concat((helper_defs, definitions)), hydra.lib.lists.cons(desc_option(), java_options()))))[1])))[1]))))

def from_either_string(cx: T0, e: Either[str, T1]) -> Either[hydra.errors.Error, T1]:
    return hydra.lib.eithers.bimap((lambda msg: cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(msg)))), (lambda a: a), e)

def module_to_protobuf(mod: hydra.packaging.Module, defs: frozenlist[hydra.packaging.Definition], cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.errors.Error, FrozenDict[str, str]]:
    r"""Convert a Hydra module to Protocol Buffers v3 source files."""

    ns_ = mod.namespace
    @lru_cache(1)
    def partitioned() -> tuple[frozenlist[hydra.packaging.TypeDefinition], frozenlist[hydra.packaging.TermDefinition]]:
        return hydra.environment.partition_definitions(defs)
    @lru_cache(1)
    def type_defs() -> frozenlist[hydra.packaging.TypeDefinition]:
        return hydra.lib.pairs.first(partitioned())
    return hydra.lib.eithers.bind(construct_module(cx, g, mod, type_defs()), (lambda pfile: (content := hydra.serialization.print_expr(hydra.serialization.parenthesize(hydra.protobuf.serde.write_proto_file(pfile))), path := namespace_to_file_reference(ns_).value, Right(hydra.lib.maps.singleton(path, content)))[2]))
