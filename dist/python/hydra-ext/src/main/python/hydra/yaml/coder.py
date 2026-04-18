# Note: this is an automatically generated file. Do not edit.

r"""YAML encoding and decoding for Hydra terms."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt
import hydra.coders
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.show.core
import hydra.strip
import hydra.yaml.language
import hydra.yaml.model

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def decode_record(tname: hydra.core.Name, coders: frozenlist[tuple[hydra.core.FieldType, hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]]], cx: hydra.context.Context, n: hydra.yaml.model.Node_) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Decode a YAML value to a record term."""

    def decode_object_body(m: FrozenDict[hydra.yaml.model.Node_, hydra.yaml.model.Node_]) -> Either[hydra.errors.Error, hydra.core.Term]:
        def decode_field(coder: tuple[hydra.core.FieldType, hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]]) -> Either[hydra.errors.Error, hydra.core.Field]:
            @lru_cache(1)
            def ft() -> hydra.core.FieldType:
                return hydra.lib.pairs.first(coder)
            @lru_cache(1)
            def coder_() -> hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]:
                return hydra.lib.pairs.second(coder)
            fname = ft().name
            default_value = cast(hydra.yaml.model.Node_, hydra.yaml.model.NodeScalar(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarNull())))
            @lru_cache(1)
            def yaml_value() -> hydra.yaml.model.Node_:
                return hydra.lib.maybes.from_maybe((lambda : default_value), hydra.lib.maps.lookup(cast(hydra.yaml.model.Node_, hydra.yaml.model.NodeScalar(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarStr(fname.value)))), m))
            return hydra.lib.eithers.bind(coder_().decode(cx, yaml_value()), (lambda v: Right(hydra.core.Field(fname, v))))
        return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: decode_field(x1)), coders), (lambda fields: Right(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(tname, fields))))))
    match n:
        case hydra.yaml.model.NodeMapping(value=v1):
            return decode_object_body(v1)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected mapping"))))

def encode_record(coders: frozenlist[tuple[hydra.core.FieldType, hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]]], cx: hydra.context.Context, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.yaml.model.Node_]:
    r"""Encode a record term to YAML."""

    @lru_cache(1)
    def stripped() -> hydra.core.Term:
        return hydra.strip.deannotate_term(term)
    def is_maybe_nothing(ft: hydra.core.FieldType, fvalue: hydra.core.Term):
        def _hoist_is_maybe_nothing_1(v1):
            match v1:
                case hydra.core.TermMaybe(value=opt):
                    return hydra.lib.maybes.is_nothing(opt)

                case _:
                    return False
        match ft.type:
            case hydra.core.TypeMaybe():
                return _hoist_is_maybe_nothing_1(fvalue)

            case _:
                return False
    def encode_field(coder_and_field: tuple[tuple[hydra.core.FieldType, hydra.coders.Coder[hydra.core.Term, T0]], hydra.core.Field]) -> Either[hydra.errors.Error, Maybe[tuple[hydra.yaml.model.Node_, T0]]]:
        @lru_cache(1)
        def ft_and_coder() -> tuple[hydra.core.FieldType, hydra.coders.Coder[hydra.core.Term, T0]]:
            return hydra.lib.pairs.first(coder_and_field)
        @lru_cache(1)
        def field() -> hydra.core.Field:
            return hydra.lib.pairs.second(coder_and_field)
        @lru_cache(1)
        def ft() -> hydra.core.FieldType:
            return hydra.lib.pairs.first(ft_and_coder())
        @lru_cache(1)
        def coder_() -> hydra.coders.Coder[hydra.core.Term, T0]:
            return hydra.lib.pairs.second(ft_and_coder())
        fname = field().name
        fvalue = field().term
        return hydra.lib.logic.if_else(is_maybe_nothing(ft(), fvalue), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.bind(coder_().encode(cx, fvalue), (lambda encoded: Right(Just((cast(hydra.yaml.model.Node_, hydra.yaml.model.NodeScalar(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarStr(fname.value)))), encoded)))))))
    return hydra.lib.eithers.bind(hydra.extract.core.term_record(graph, stripped()), (lambda record: (fields := record.fields, hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: encode_field(x1)), hydra.lib.lists.zip(coders, fields)), (lambda maybe_fields: Right(cast(hydra.yaml.model.Node_, hydra.yaml.model.NodeMapping(hydra.lib.maps.from_list(hydra.lib.maybes.cat(maybe_fields))))))))[1]))

def requires_yaml_string_sentinel(s: str) -> bool:
    r"""True for IEEE sentinel strings that Hydra YAML cannot represent as a float scalar."""

    return hydra.lib.logic.or_(hydra.lib.equality.equal(s, "NaN"), hydra.lib.logic.or_(hydra.lib.equality.equal(s, "Infinity"), hydra.lib.logic.or_(hydra.lib.equality.equal(s, "-Infinity"), hydra.lib.equality.equal(s, "-0.0"))))

def literal_yaml_coder(lt: hydra.core.LiteralType) -> Either[T0, hydra.coders.Coder[hydra.core.Literal, hydra.yaml.model.Scalar]]:
    r"""Create a YAML coder for literal types."""

    def decode_bool(cx: T1, s: hydra.yaml.model.Scalar) -> Either[hydra.errors.Error, hydra.core.Literal]:
        match s:
            case hydra.yaml.model.ScalarBool(value=b):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralBoolean(b)))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected boolean, found scalar",))))))
    def decode_decimal(cx: T1, s: hydra.yaml.model.Scalar) -> Either[hydra.errors.Error, hydra.core.Literal]:
        match s:
            case hydra.yaml.model.ScalarDecimal(value=d):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralDecimal(d)))

            case hydra.yaml.model.ScalarFloat(value=f):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralDecimal(hydra.lib.literals.float64_to_decimal(hydra.lib.literals.bigfloat_to_float64(f)))))

            case hydra.yaml.model.ScalarInt(value=i):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralDecimal(hydra.lib.literals.bigint_to_decimal(i))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected decimal, found scalar",))))))
    def decode_float(cx: T1, s: hydra.yaml.model.Scalar) -> Either[hydra.errors.Error, hydra.core.Literal]:
        match s:
            case hydra.yaml.model.ScalarDecimal(value=d):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(hydra.lib.literals.float64_to_bigfloat(hydra.lib.literals.decimal_to_float64(d)))))))

            case hydra.yaml.model.ScalarFloat(value=f):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralFloat(cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(f)))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected float, found scalar",))))))
    def decode_integer(cx: T1, s: hydra.yaml.model.Scalar) -> Either[hydra.errors.Error, hydra.core.Literal]:
        match s:
            case hydra.yaml.model.ScalarInt(value=i):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(i)))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected integer, found scalar",))))))
    def decode_string(cx: T1, s: hydra.yaml.model.Scalar) -> Either[hydra.errors.Error, hydra.core.Literal]:
        match s:
            case hydra.yaml.model.ScalarStr(value=s_):
                return Right(cast(hydra.core.Literal, hydra.core.LiteralString(s_)))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected string, found scalar",))))))
    @lru_cache(1)
    def encoded() -> hydra.coders.Coder[hydra.core.Literal, hydra.yaml.model.Scalar]:
        match lt:
            case hydra.core.LiteralTypeBoolean():
                return hydra.coders.Coder((lambda cx, lit: hydra.lib.eithers.bind(hydra.extract.core.boolean_literal(lit), (lambda b: Right(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarBool(b)))))), (lambda x1, x2: decode_bool(x1, x2)))

            case hydra.core.LiteralTypeDecimal():
                return hydra.coders.Coder((lambda cx, lit: hydra.lib.eithers.bind(hydra.extract.core.decimal_literal(lit), (lambda d: Right(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarDecimal(d)))))), (lambda x1, x2: decode_decimal(x1, x2)))

            case hydra.core.LiteralTypeFloat():
                return hydra.coders.Coder((lambda cx, lit: hydra.lib.eithers.bind(hydra.extract.core.float_literal(lit), (lambda f: hydra.lib.eithers.bind(hydra.extract.core.bigfloat_value(f), (lambda bf: (shown := hydra.lib.literals.show_bigfloat(bf), hydra.lib.logic.if_else(requires_yaml_string_sentinel(shown), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("YAML cannot represent bigfloat value: ", shown))))))), (lambda : Right(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarFloat(bf))))))[1]))))), (lambda x1, x2: decode_float(x1, x2)))

            case hydra.core.LiteralTypeInteger():
                return hydra.coders.Coder((lambda cx, lit: hydra.lib.eithers.bind(hydra.extract.core.integer_literal(lit), (lambda i: hydra.lib.eithers.bind(hydra.extract.core.bigint_value(i), (lambda bi: Right(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarInt(bi)))))))), (lambda x1, x2: decode_integer(x1, x2)))

            case hydra.core.LiteralTypeString():
                return hydra.coders.Coder((lambda cx, lit: hydra.lib.eithers.bind(hydra.extract.core.string_literal(lit), (lambda s: Right(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarStr(s)))))), (lambda x1, x2: decode_string(x1, x2)))

            case _:
                raise TypeError("Unsupported LiteralType")
    return Right(encoded())

@lru_cache(1)
def unit_coder() -> hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]:
    r"""YAML coder for unit values."""

    def encode_unit(cx: T0, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.yaml.model.Node_]:
        match hydra.strip.deannotate_term(term):
            case hydra.core.TermUnit():
                return Right(cast(hydra.yaml.model.Node_, hydra.yaml.model.NodeScalar(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarNull()))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected unit, found: ", hydra.show.core.term(term)))))))
    def decode_unit(cx: T0, n: hydra.yaml.model.Node_) -> Either[hydra.errors.Error, hydra.core.Term]:
        match n:
            case hydra.yaml.model.NodeScalar(value=s):
                match s:
                    case hydra.yaml.model.ScalarNull():
                        return Right(cast(hydra.core.Term, hydra.core.TermUnit()))

                    case _:
                        return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected null scalar"))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected null"))))
    return hydra.coders.Coder((lambda x1, x2: encode_unit(x1, x2)), (lambda x1, x2: decode_unit(x1, x2)))

def record_coder(tname: hydra.core.Name, rt: frozenlist[hydra.core.FieldType], cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]]:
    r"""Create a YAML coder for record types."""

    def get_coder(f: hydra.core.FieldType) -> Either[hydra.errors.Error, tuple[hydra.core.FieldType, hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]]]:
        return hydra.lib.eithers.bind(term_coder(f.type, cx, g), (lambda coder: Right((f, coder))))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda x1: get_coder(x1)), rt), (lambda coders: Right(hydra.coders.Coder((lambda cx2, term: encode_record(coders, cx2, g, term)), (lambda cx2, val: decode_record(tname, coders, cx2, val))))))

def term_coder(typ: hydra.core.Type, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]]:
    r"""Create a YAML coder for term types."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    def encode_literal(ac: hydra.coders.Coder[hydra.core.Literal, hydra.yaml.model.Scalar], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.yaml.model.Node_]:
        match term:
            case hydra.core.TermLiteral(value=av):
                return hydra.lib.eithers.bind(ac.encode(cx2, av), (lambda scalar: Right(cast(hydra.yaml.model.Node_, hydra.yaml.model.NodeScalar(scalar)))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected literal term, found: ", hydra.show.core.term(term)))))))
    def encode_list(lc: hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_], cx2: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.yaml.model.Node_]:
        match term:
            case hydra.core.TermList(value=els):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda el: lc.encode(cx2, el)), els), (lambda encoded_els: Right(cast(hydra.yaml.model.Node_, hydra.yaml.model.NodeSequence(encoded_els)))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected list term, found: ", hydra.show.core.term(term)))))))
    def decode_list(lc: hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_], cx2: hydra.context.Context, n: hydra.yaml.model.Node_) -> Either[hydra.errors.Error, hydra.core.Term]:
        match n:
            case hydra.yaml.model.NodeSequence(value=nodes):
                return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda node: lc.decode(cx2, node)), nodes), (lambda decoded_nodes: Right(cast(hydra.core.Term, hydra.core.TermList(decoded_nodes)))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected sequence"))))
    def encode_maybe(maybe_element_coder: hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_], cx2: hydra.context.Context, maybe_term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.yaml.model.Node_]:
        @lru_cache(1)
        def stripped_maybe_term() -> hydra.core.Term:
            return hydra.strip.deannotate_term(maybe_term)
        match stripped_maybe_term():
            case hydra.core.TermMaybe(value=maybe_contents):
                return hydra.lib.maybes.maybe((lambda : Right(cast(hydra.yaml.model.Node_, hydra.yaml.model.NodeScalar(cast(hydra.yaml.model.Scalar, hydra.yaml.model.ScalarNull()))))), (lambda inner_term: hydra.lib.eithers.bind(maybe_element_coder.encode(cx2, inner_term), (lambda encoded_inner: Right(encoded_inner)))), maybe_contents)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected optional term, found: ", hydra.show.core.term(maybe_term)))))))
    def decode_maybe(maybe_element_coder: hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_], cx2: hydra.context.Context, yaml_val: hydra.yaml.model.Node_):
        def _hoist_decode_maybe_1(cx2, maybe_element_coder, yaml_val, v1):
            match v1:
                case hydra.yaml.model.ScalarNull():
                    return Right(cast(hydra.core.Term, hydra.core.TermMaybe(Nothing())))

                case _:
                    return hydra.lib.eithers.bind(maybe_element_coder.decode(cx2, yaml_val), (lambda decoded_inner: Right(cast(hydra.core.Term, hydra.core.TermMaybe(Just(decoded_inner))))))
        match yaml_val:
            case hydra.yaml.model.NodeScalar(value=s):
                return _hoist_decode_maybe_1(cx2, maybe_element_coder, yaml_val, s)

            case _:
                return hydra.lib.eithers.bind(maybe_element_coder.decode(cx2, yaml_val), (lambda decoded_inner: Right(cast(hydra.core.Term, hydra.core.TermMaybe(Just(decoded_inner))))))
    @lru_cache(1)
    def result():
        def _hoist_result_1(ac, cx2, v1):
            match v1:
                case hydra.yaml.model.NodeScalar(value=s):
                    return hydra.lib.eithers.bind(ac.decode(cx2, s), (lambda lit: Right(cast(hydra.core.Term, hydra.core.TermLiteral(lit)))))

                case _:
                    return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected scalar node"))))
        match stripped():
            case hydra.core.TypeLiteral(value=at):
                return hydra.lib.eithers.bind(literal_yaml_coder(at), (lambda ac: Right(hydra.coders.Coder((lambda v1, v2: encode_literal(ac, v1, v2)), (lambda cx2, n: _hoist_result_1(ac, cx2, n))))))

            case hydra.core.TypeList(value=lt):
                return hydra.lib.eithers.bind(term_coder(lt, cx, g), (lambda lc: Right(hydra.coders.Coder((lambda v1, v2: encode_list(lc, v1, v2)), (lambda v1, v2: decode_list(lc, v1, v2))))))

            case hydra.core.TypeMap(value=mt):
                kt = mt.keys
                vt = mt.values
                return hydra.lib.eithers.bind(term_coder(kt, cx, g), (lambda kc: hydra.lib.eithers.bind(term_coder(vt, cx, g), (lambda vc: (encode_entry := (lambda cx2, kv: (k := hydra.lib.pairs.first(kv), v := hydra.lib.pairs.second(kv), hydra.lib.eithers.bind(kc.encode(cx2, k), (lambda encoded_k: hydra.lib.eithers.bind(vc.encode(cx2, v), (lambda encoded_v: Right((encoded_k, encoded_v)))))))[2]), decode_entry := (lambda cx2, kv: (k := hydra.lib.pairs.first(kv), v := hydra.lib.pairs.second(kv), hydra.lib.eithers.bind(kc.decode(cx2, k), (lambda decoded_k: hydra.lib.eithers.bind(vc.decode(cx2, v), (lambda decoded_v: Right((decoded_k, decoded_v)))))))[2]), _hoist_decode_entry_body_1 := (lambda cx2, term, v1: (lambda m: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda entry: encode_entry(cx2, entry)), hydra.lib.maps.to_list(m)), (lambda entries: Right(cast(hydra.yaml.model.Node_, hydra.yaml.model.NodeMapping(hydra.lib.maps.from_list(entries)))))))(v1.value) if isinstance(v1, hydra.core.TermMap) else Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("expected map term, found: ", hydra.show.core.term(term)))))))), _hoist_decode_entry_body_2 := (lambda cx2, v1: (lambda m: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda entry: decode_entry(cx2, entry)), hydra.lib.maps.to_list(m)), (lambda entries: Right(cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.from_list(entries)))))))(v1.value) if isinstance(v1, hydra.yaml.model.NodeMapping) else Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("expected mapping"))))), Right(hydra.coders.Coder((lambda cx2, term: _hoist_decode_entry_body_1(cx2, term, term)), (lambda cx2, n: _hoist_decode_entry_body_2(cx2, n)))))[4]))))

            case hydra.core.TypeMaybe(value=maybe_element_type):
                return hydra.lib.eithers.bind(term_coder(maybe_element_type, cx, g), (lambda maybe_element_coder: Right(hydra.coders.Coder((lambda v1, v2: encode_maybe(maybe_element_coder, v1, v2)), (lambda v1, v2: decode_maybe(maybe_element_coder, v1, v2))))))

            case hydra.core.TypeRecord(value=rt):
                return record_coder(hydra.core.Name("yaml"), rt, cx, g)

            case hydra.core.TypeUnit():
                return Right(unit_coder())

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat(("unsupported type in YAML: ", hydra.show.core.type(typ)))))))
    return result()

def yaml_coder(typ: hydra.core.Type, cx: T0, g: hydra.graph.Graph) -> Either[hydra.errors.Error, hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]]:
    r"""Create a YAML coder for a given type."""

    def mk_term_coder(t: hydra.core.Type) -> Either[hydra.errors.Error, hydra.coders.Coder[hydra.core.Term, hydra.yaml.model.Node_]]:
        return term_coder(t, cx, g)
    return hydra.lib.eithers.bind(hydra.adapt.simple_language_adapter(hydra.yaml.language.yaml_language(), cx, g, typ), (lambda adapter: hydra.lib.eithers.bind(mk_term_coder(adapter.target), (lambda coder: Right(hydra.adapt.compose_coders(adapter.coder, coder))))))
