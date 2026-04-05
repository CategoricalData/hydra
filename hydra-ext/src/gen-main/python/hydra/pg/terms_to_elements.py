# Note: this is an automatically generated file. Do not edit.

r"""Functions for mapping Hydra terms to property graph elements using mapping specifications."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.coders
import hydra.context
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
import hydra.pg.mapping
import hydra.pg.model
import hydra.resolution
import hydra.show.core
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")
T4 = TypeVar("T4")
T5 = TypeVar("T5")

def eval_step(cx: hydra.context.Context, step: str, term: hydra.core.Term):
    def _hoist_hydra_pg_terms_to_elements_eval_step_1(cx, step, v1):
        match v1:
            case hydra.core.TermList(value=terms):
                return hydra.lib.eithers.map((lambda xs: hydra.lib.lists.concat(xs)), hydra.lib.eithers.map_list((lambda v12: eval_step(cx, step, v12)), terms))

            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe((lambda : Right(())), (lambda t: eval_step(cx, step, t)), mt)

            case hydra.core.TermRecord(value=rec):
                return hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2(hydra.lib.strings.cat2("No such field ", step), " in record")))), cx))), (lambda t: Right((t,))), hydra.lib.maps.lookup(hydra.core.Name(step), hydra.resolution.field_map(rec.fields)))

            case hydra.core.TermUnion(value=inj):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(inj.field.name.value, step), (lambda : eval_step(cx, step, inj.field.term)), (lambda : Right(())))

            case hydra.core.TermWrap(value=wt):
                return eval_step(cx, step, wt.body)

            case _:
                return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Can't traverse through term for step ", step)))), cx))
    return hydra.lib.logic.if_else(hydra.lib.strings.null(step), (lambda : Right((term,))), (lambda : _hoist_hydra_pg_terms_to_elements_eval_step_1(cx, step, hydra.strip.deannotate_term(term))))

def eval_path(cx: hydra.context.Context, path: frozenlist[str], term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.core.Term]]:
    r"""Evaluate a path (list of steps) on a term, returning all resulting terms."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(path), (lambda : Right((term,))), (lambda : hydra.lib.eithers.bind(eval_step(cx, hydra.lib.lists.head(path), term), (lambda results: hydra.lib.eithers.map((lambda xs: hydra.lib.lists.concat(xs)), hydra.lib.eithers.map_list((lambda v1: eval_path(cx, hydra.lib.lists.tail(path), v1)), results))))))

def term_to_string(term: hydra.core.Term):
    def _hoist_hydra_pg_terms_to_elements_term_to_string_1(term, v1):
        match v1:
            case hydra.core.IntegerValueInt32(value=n):
                return hydra.lib.literals.show_int32(n)

            case _:
                return hydra.show.core.term(term)
    def _hoist_hydra_pg_terms_to_elements_term_to_string_2(term, v1):
        match v1:
            case hydra.core.FloatValueFloat64(value=n):
                return hydra.lib.literals.show_float64(n)

            case _:
                return hydra.show.core.term(term)
    def _hoist_hydra_pg_terms_to_elements_term_to_string_3(term, v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return s

            case hydra.core.LiteralBoolean(value=b):
                return hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false"))

            case hydra.core.LiteralInteger(value=i):
                return _hoist_hydra_pg_terms_to_elements_term_to_string_1(term, i)

            case hydra.core.LiteralFloat(value=f):
                return _hoist_hydra_pg_terms_to_elements_term_to_string_2(term, f)

            case _:
                return hydra.show.core.term(term)
    match hydra.strip.deannotate_term(term):
        case hydra.core.TermLiteral(value=lit):
            return _hoist_hydra_pg_terms_to_elements_term_to_string_3(term, lit)

        case hydra.core.TermMaybe(value=mt):
            return hydra.lib.maybes.maybe((lambda : "nothing"), (lambda t: term_to_string(t)), mt)

        case _:
            return hydra.show.core.term(term)

def apply_pattern(cx: hydra.context.Context, first_lit: str, pairs: frozenlist[tuple[frozenlist[str], str]], term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.core.Term]]:
    r"""Apply a parsed pattern to a term, producing string terms."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(pairs), (lambda : Right((cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(first_lit)))),))), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda pp: hydra.lib.eithers.map((lambda terms: (hydra.lib.lists.map((lambda t: term_to_string(t)), terms), hydra.lib.pairs.second(pp))), eval_path(cx, hydra.lib.pairs.first(pp), term))), pairs), (lambda evaluated: Right(hydra.lib.lists.map((lambda s: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))), hydra.lib.lists.foldl((lambda accum, ep: (p_strs := hydra.lib.pairs.first(ep), lit_p := hydra.lib.pairs.second(ep), hydra.lib.lists.concat(hydra.lib.lists.map((lambda p_str: hydra.lib.lists.map((lambda a: hydra.lib.strings.cat2(hydra.lib.strings.cat2(a, p_str), lit_p)), accum)), p_strs)))[2]), (first_lit,), evaluated)))))))

def decode_edge_label(cx: hydra.context.Context, g: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.pg.model.EdgeLabel]:
    r"""Decode an edge label from a term."""

    return hydra.lib.eithers.map((lambda _x: hydra.pg.model.EdgeLabel(_x)), hydra.extract.core.string(cx, g, t))

def decode_property_key(cx: hydra.context.Context, g: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.pg.model.PropertyKey]:
    r"""Decode a property key from a term."""

    return hydra.lib.eithers.map((lambda _x: hydra.pg.model.PropertyKey(_x)), hydra.extract.core.string(cx, g, t))

def read_injection(cx: hydra.context.Context, g: hydra.graph.Graph, cases: frozenlist[tuple[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], T0]]]], encoded: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], T0]:
    r"""Read an injection (union value) from a term."""

    return hydra.lib.eithers.bind(hydra.extract.core.map(cx, (lambda k: hydra.lib.eithers.map((lambda _n: hydra.core.Name(_n)), hydra.extract.core.string(cx, g, k))), (lambda _v: Right(_v)), g, encoded), (lambda mp: (entries := hydra.lib.maps.to_list(mp), hydra.lib.logic.if_else(hydra.lib.lists.null(entries), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("empty injection"))), cx))), (lambda : (f := hydra.lib.lists.head(entries), key := hydra.lib.pairs.first(f), val := hydra.lib.pairs.second(f), matching := hydra.lib.lists.filter((lambda c: hydra.lib.equality.equal(hydra.lib.pairs.first(c), key)), cases), hydra.lib.logic.if_else(hydra.lib.lists.null(matching), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("unexpected field: ", key.value)))), cx))), (lambda : (lambda : hydra.lib.pairs.second(hydra.lib.lists.head(matching), val)))))[4])))[1]))

def decode_value_spec(cx: hydra.context.Context, g: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_pg_terms_to_elements_decode_value_spec_1(cx, g, term, v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(cast(hydra.pg.mapping.ValueSpec, hydra.pg.mapping.ValueSpecPattern(s)))

            case _:
                return read_injection(cx, g, ((hydra.core.Name("value"), (lambda _: Right(cast(hydra.pg.mapping.ValueSpec, hydra.pg.mapping.ValueSpecValue())))), (hydra.core.Name("pattern"), (lambda t: hydra.lib.eithers.map((lambda _x: cast(hydra.pg.mapping.ValueSpec, hydra.pg.mapping.ValueSpecPattern(_x))), hydra.extract.core.string(cx, g, t))))), term)
    match hydra.strip.deannotate_term(term):
        case hydra.core.TermLiteral(value=lit):
            return _hoist_hydra_pg_terms_to_elements_decode_value_spec_1(cx, g, term, lit)

        case _:
            return read_injection(cx, g, ((hydra.core.Name("value"), (lambda _: Right(cast(hydra.pg.mapping.ValueSpec, hydra.pg.mapping.ValueSpecValue())))), (hydra.core.Name("pattern"), (lambda t: hydra.lib.eithers.map((lambda _x: cast(hydra.pg.mapping.ValueSpec, hydra.pg.mapping.ValueSpecPattern(_x))), hydra.extract.core.string(cx, g, t))))), term)

def read_field(cx: hydra.context.Context, fields: FrozenDict[hydra.core.Name, T0], fname: hydra.core.Name, fun: Callable[[T0], Either[hydra.context.InContext[hydra.errors.Error], T1]]) -> Either[hydra.context.InContext[hydra.errors.Error], T1]:
    r"""Read a field from a map of fields by name."""

    return hydra.lib.maybes.maybe((lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("no such field: ", fname.value)))), cx))), fun, hydra.lib.maps.lookup(fname, fields))

def read_record(cx: hydra.context.Context, g: hydra.graph.Graph, cons: Callable[[FrozenDict[hydra.core.Name, hydra.core.Term]], Either[hydra.context.InContext[hydra.errors.Error], T0]], term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], T0]:
    r"""Read a record from a term as a map of field names to values."""

    return hydra.lib.eithers.bind(hydra.extract.core.map(cx, (lambda k: hydra.lib.eithers.map((lambda _n: hydra.core.Name(_n)), hydra.extract.core.string(cx, g, k))), (lambda _v: Right(_v)), g, term), cons)

def decode_property_spec(cx: hydra.context.Context, g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.pg.mapping.PropertySpec]:
    r"""Decode a property specification from a term."""

    return read_record(cx, g, (lambda fields: hydra.lib.eithers.bind(read_field(cx, fields, hydra.core.Name("key"), (lambda v1: decode_property_key(cx, g, v1))), (lambda _a: hydra.lib.eithers.map((lambda _b: hydra.pg.mapping.PropertySpec(_a, _b)), read_field(cx, fields, hydra.core.Name("value"), (lambda v1: decode_value_spec(cx, g, v1))))))), term)

def expect_list(cx: hydra.context.Context, g: hydra.graph.Graph, f: Callable[[hydra.context.Context, hydra.graph.Graph, hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], T0]], term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], frozenlist[T0]]:
    r"""Extract a list from a term and apply a decoder to each element."""

    return hydra.lib.eithers.bind(hydra.extract.core.list(cx, g, term), (lambda elems: hydra.lib.eithers.map_list((lambda v1: f(cx, g, v1)), elems)))

def decode_edge_spec(cx: hydra.context.Context, g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.pg.mapping.EdgeSpec]:
    r"""Decode an edge specification from a term."""

    return read_record(cx, g, (lambda fields: hydra.lib.eithers.bind(read_field(cx, fields, hydra.core.Name("label"), (lambda v1: decode_edge_label(cx, g, v1))), (lambda _a: hydra.lib.eithers.bind(read_field(cx, fields, hydra.core.Name("id"), (lambda v1: decode_value_spec(cx, g, v1))), (lambda _b: hydra.lib.eithers.bind(read_field(cx, fields, hydra.core.Name("out"), (lambda v1: decode_value_spec(cx, g, v1))), (lambda _c: hydra.lib.eithers.bind(read_field(cx, fields, hydra.core.Name("in"), (lambda v1: decode_value_spec(cx, g, v1))), (lambda _d: hydra.lib.eithers.map((lambda _e: hydra.pg.mapping.EdgeSpec(_a, _b, _c, _d, _e)), read_field(cx, fields, hydra.core.Name("properties"), (lambda v1: expect_list(cx, g, (lambda x1, x2, x3: decode_property_spec(x1, x2, x3)), v1))))))))))))), term)

def decode_vertex_label(cx: hydra.context.Context, g: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.pg.model.VertexLabel]:
    r"""Decode a vertex label from a term."""

    return hydra.lib.eithers.map((lambda _x: hydra.pg.model.VertexLabel(_x)), hydra.extract.core.string(cx, g, t))

def decode_vertex_spec(cx: hydra.context.Context, g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.pg.mapping.VertexSpec]:
    r"""Decode a vertex specification from a term."""

    return read_record(cx, g, (lambda fields: hydra.lib.eithers.bind(read_field(cx, fields, hydra.core.Name("label"), (lambda v1: decode_vertex_label(cx, g, v1))), (lambda _a: hydra.lib.eithers.bind(read_field(cx, fields, hydra.core.Name("id"), (lambda v1: decode_value_spec(cx, g, v1))), (lambda _b: hydra.lib.eithers.map((lambda _c: hydra.pg.mapping.VertexSpec(_a, _b, _c)), read_field(cx, fields, hydra.core.Name("properties"), (lambda v1: expect_list(cx, g, (lambda x1, x2, x3: decode_property_spec(x1, x2, x3)), v1))))))))), term)

def decode_element_spec(cx: hydra.context.Context, g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.pg.mapping.ElementSpec]:
    r"""Decode an element specification from a term."""

    return read_injection(cx, g, ((hydra.core.Name("vertex"), (lambda t: hydra.lib.eithers.map((lambda _x: cast(hydra.pg.mapping.ElementSpec, hydra.pg.mapping.ElementSpecVertex(_x))), decode_vertex_spec(cx, g, t)))), (hydra.core.Name("edge"), (lambda t: hydra.lib.eithers.map((lambda _x: cast(hydra.pg.mapping.ElementSpec, hydra.pg.mapping.ElementSpecEdge(_x))), decode_edge_spec(cx, g, t))))), term)

def parse_pattern(cx: T0, _g: T1, pat: str) -> Either[T2, Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.core.Term]]]]:
    r"""Parse a string pattern into a function that traverses terms."""

    @lru_cache(1)
    def segments() -> frozenlist[str]:
        return hydra.lib.strings.split_on("${", pat)
    @lru_cache(1)
    def first_lit() -> str:
        return hydra.lib.lists.head(segments())
    @lru_cache(1)
    def rest() -> frozenlist[str]:
        return hydra.lib.lists.tail(segments())
    @lru_cache(1)
    def parsed() -> frozenlist[tuple[frozenlist[str], str]]:
        return hydra.lib.lists.map((lambda seg: (parts := hydra.lib.strings.split_on("}", seg), path_str := hydra.lib.lists.head(parts), lit_part := hydra.lib.strings.intercalate("}", hydra.lib.lists.tail(parts)), path_steps := hydra.lib.strings.split_on("/", path_str), (path_steps, lit_part))[4]), rest())
    return Right((lambda cx_, term: apply_pattern(cx_, first_lit(), parsed(), term)))

def parse_value_spec(cx: T0, g: T1, spec: hydra.pg.mapping.ValueSpec) -> Either[T2, Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.core.Term]]]]:
    r"""Parse a value specification into a function that processes terms."""

    match spec:
        case hydra.pg.mapping.ValueSpecValue():
            return Right((lambda _cx, term: Right((term,))))

        case hydra.pg.mapping.ValueSpecPattern(value=pat):
            return parse_pattern(cx, g, pat)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def parse_edge_id_pattern(cx: T0, g: T1, schema: hydra.pg.mapping.Schema[T2, T3, T4], spec: hydra.pg.mapping.ValueSpec) -> Either[T5, Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], frozenlist[T4]]]]:
    r"""Parse an edge id pattern from a value spec and schema."""

    return hydra.lib.eithers.bind(parse_value_spec(cx, g, spec), (lambda fun: Right((lambda cx_, term: hydra.lib.eithers.bind(fun(cx_, term), (lambda terms: hydra.lib.eithers.map_list(schema.edge_ids.encode(cx_), terms)))))))

def parse_property_spec(cx: T0, g: T1, schema: hydra.pg.mapping.Schema[T2, T3, T4], spec: hydra.pg.mapping.PropertySpec) -> Either[T5, Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], frozenlist[tuple[hydra.pg.model.PropertyKey, T4]]]]]:
    r"""Parse a property specification into an encoder function."""

    key = spec.key
    value = spec.value
    return hydra.lib.eithers.bind(parse_value_spec(cx, g, value), (lambda fun: Right((lambda cx_, term: hydra.lib.eithers.bind(fun(cx_, term), (lambda results: hydra.lib.eithers.bind(hydra.lib.eithers.map_list(schema.property_values.encode(cx_), results), (lambda values: Right(hydra.lib.lists.map((lambda v: (key, v)), values))))))))))

def parse_vertex_id_pattern(cx: T0, g: T1, schema: hydra.pg.mapping.Schema[T2, T3, T4], spec: hydra.pg.mapping.ValueSpec) -> Either[T5, Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], frozenlist[T4]]]]:
    r"""Parse a vertex id pattern from a value spec and schema."""

    return hydra.lib.eithers.bind(parse_value_spec(cx, g, spec), (lambda fun: Right((lambda cx_, term: hydra.lib.eithers.bind(fun(cx_, term), (lambda terms: hydra.lib.eithers.map_list(schema.vertex_ids.encode(cx_), terms)))))))

def require_unique(cx: hydra.context.Context, context: str, fun: Callable[[T0], Either[hydra.context.InContext[hydra.errors.Error], frozenlist[T1]]], term: T0) -> Either[hydra.context.InContext[hydra.errors.Error], T1]:
    r"""Require exactly one result from a list-producing function."""

    return hydra.lib.eithers.bind(fun(term), (lambda results: hydra.lib.logic.if_else(hydra.lib.lists.null(results), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("No value found: ", context)))), cx))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(results), 1), (lambda : Right(hydra.lib.lists.head(results))), (lambda : Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(hydra.lib.strings.cat2("Multiple values found: ", context)))), cx))))))))

def parse_edge_spec(cx: T0, g: T1, schema: hydra.pg.mapping.Schema[T2, T3, T4], spec: hydra.pg.mapping.EdgeSpec) -> Either[T5, tuple[hydra.pg.model.Label, Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.pg.model.Element[T4]]]]]]:
    r"""Parse an edge specification into a label and encoder function."""

    label = spec.label
    id = spec.id
    out_v = spec.out
    in_v = spec.in_
    props = spec.properties
    return hydra.lib.eithers.bind(parse_edge_id_pattern(cx, g, schema, id), (lambda get_id: hydra.lib.eithers.bind(parse_vertex_id_pattern(cx, g, schema, out_v), (lambda get_out: hydra.lib.eithers.bind(parse_vertex_id_pattern(cx, g, schema, in_v), (lambda get_in: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: parse_property_spec(cx, g, schema, v1)), props), (lambda get_props: Right((cast(hydra.pg.model.Label, hydra.pg.model.LabelEdge(label)), (lambda cx_, term: hydra.lib.eithers.bind(require_unique(cx_, "edge id", (lambda v1: get_id(cx_, v1)), term), (lambda tid: hydra.lib.eithers.bind(require_unique(cx_, "vertex id", (lambda v1: get_out(cx_, v1)), term), (lambda tout: hydra.lib.eithers.bind(require_unique(cx_, "edge id", (lambda v1: get_in(cx_, v1)), term), (lambda tin: hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda _xs: hydra.lib.maps.from_list(_xs)), hydra.lib.eithers.map_list((lambda gf: require_unique(cx_, "property key", (lambda v1: gf(cx_, v1)), term)), get_props)), (lambda tprops: Right((cast(hydra.pg.model.Element, hydra.pg.model.ElementEdge(hydra.pg.model.Edge(label, tid, tout, tin, tprops))),)))))))))))))))))))))

def parse_vertex_spec(cx: T0, g: T1, schema: hydra.pg.mapping.Schema[T2, T3, T4], spec: hydra.pg.mapping.VertexSpec) -> Either[T5, tuple[hydra.pg.model.Label, Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.pg.model.Element[T4]]]]]]:
    r"""Parse a vertex specification into a label and encoder function."""

    label = spec.label
    id = spec.id
    props = spec.properties
    return hydra.lib.eithers.bind(parse_vertex_id_pattern(cx, g, schema, id), (lambda get_id: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: parse_property_spec(cx, g, schema, v1)), props), (lambda get_props: Right((cast(hydra.pg.model.Label, hydra.pg.model.LabelVertex(label)), (lambda cx_, term: hydra.lib.eithers.bind(require_unique(cx_, "vertex id", (lambda v1: get_id(cx_, v1)), term), (lambda tid: hydra.lib.eithers.bind(hydra.lib.eithers.map((lambda _xs: hydra.lib.maps.from_list(_xs)), hydra.lib.eithers.map_list((lambda gf: require_unique(cx_, "property key", (lambda v1: gf(cx_, v1)), term)), get_props)), (lambda tprops: Right((cast(hydra.pg.model.Element, hydra.pg.model.ElementVertex(hydra.pg.model.Vertex(label, tid, tprops))),)))))))))))))

def parse_element_spec(cx: T0, g: T1, schema: hydra.pg.mapping.Schema[T2, T3, T4], spec: hydra.pg.mapping.ElementSpec) -> Either[T5, tuple[hydra.pg.model.Label, Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.errors.Error], frozenlist[hydra.pg.model.Element[T4]]]]]]:
    r"""Parse an element specification into a label and encoder function."""

    match spec:
        case hydra.pg.mapping.ElementSpecVertex(value=vspec):
            return parse_vertex_spec(cx, g, schema, vspec)

        case hydra.pg.mapping.ElementSpecEdge(value=espec):
            return parse_edge_spec(cx, g, schema, espec)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def term_to_elements_adapter(cx: hydra.context.Context, g: hydra.graph.Graph, schema: hydra.pg.mapping.Schema[T0, T1, T2], typ: hydra.core.Type) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.coders.Adapter[hydra.core.Type, frozenlist[hydra.pg.model.Label], hydra.core.Term, frozenlist[hydra.pg.model.Element[T2]]]]:
    r"""Create an adapter that maps terms to property graph elements using a mapping specification."""

    key_elements = hydra.core.Name("elements")
    return hydra.lib.maybes.maybe((lambda : Right(hydra.coders.Adapter(False, typ, (), hydra.coders.Coder((lambda _cx, _t: Right(())), (lambda cx_, _els: Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("no corresponding element type"))), cx_))))))), (lambda term: hydra.lib.eithers.bind(expect_list(cx, g, (lambda x1, x2, x3: decode_element_spec(x1, x2, x3)), term), (lambda spec_terms: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: parse_element_spec(cx, g, schema, v1)), spec_terms), (lambda specs: (labels := hydra.lib.lists.nub(hydra.lib.lists.map((lambda _p: hydra.lib.pairs.first(_p)), specs)), encoders := hydra.lib.lists.map((lambda _p: hydra.lib.pairs.second(_p)), specs), Right(hydra.coders.Adapter(False, typ, labels, hydra.coders.Coder((lambda cx_, t: hydra.lib.eithers.map((lambda _xs: hydra.lib.lists.concat(_xs)), hydra.lib.eithers.map_list((lambda e: e(cx_, t)), encoders))), (lambda cx_, _els: Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("element decoding is not yet supported"))), cx_)))))))[2]))))), hydra.annotations.get_type_annotation(key_elements, typ))
