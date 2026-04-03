# Note: this is an automatically generated file. Do not edit.

r"""SHACL coder: converts Hydra types and terms to SHACL shapes and RDF descriptions."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.annotations
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.errors
import hydra.ext.org.w3.rdf.syntax
import hydra.ext.org.w3.shacl.model
import hydra.ext.rdf.utils
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.math
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def common(constraints: frozenlist[hydra.ext.org.w3.shacl.model.CommonConstraint]) -> hydra.ext.org.w3.shacl.model.CommonProperties:
    r"""Construct CommonProperties from a list of constraints, using defaults for other fields."""

    return hydra.ext.org.w3.shacl.model.CommonProperties(hydra.lib.sets.from_list(constraints), Nothing(), hydra.ext.org.w3.rdf.syntax.LangStrings(hydra.lib.maps.empty()), hydra.ext.org.w3.shacl.model.Severity.INFO, hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty(), hydra.lib.sets.empty())

@lru_cache(1)
def default_common_properties() -> hydra.ext.org.w3.shacl.model.CommonProperties:
    r"""Default CommonProperties with empty constraints and default severity."""

    return common(())

def element_iri(el: hydra.core.Binding) -> hydra.ext.org.w3.rdf.syntax.Iri:
    r"""Convert a binding's name to an RDF IRI."""

    return hydra.ext.rdf.utils.name_to_iri(el.name)

def fold_accum_result(f: Callable[[T0, T1], Either[T2, tuple[T3, T0]]], cx: T0, xs: frozenlist[T1]) -> Either[T2, tuple[frozenlist[T3], T0]]:
    r"""Fold over a list, accumulating results and threading context through each step."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(xs), (lambda : Right(((), cx))), (lambda : hydra.lib.eithers.bind(f(cx, hydra.lib.lists.head(xs)), (lambda _r: hydra.lib.eithers.map((lambda _rest: (hydra.lib.lists.cons(hydra.lib.pairs.first(_r), hydra.lib.pairs.first(_rest)), hydra.lib.pairs.second(_rest))), fold_accum_result(f, hydra.lib.pairs.second(_r), hydra.lib.lists.tail(xs)))))))

def err(cx: hydra.context.Context, msg: str) -> Either[hydra.context.InContext[hydra.errors.Error], T0]:
    r"""Construct an error result with a context and message."""

    return Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(msg))), cx))

def unexpected_e(cx: hydra.context.Context, expected: str, found: str) -> Either[hydra.context.InContext[hydra.errors.Error], T0]:
    r"""Construct an error for unexpected input, given expected and found descriptions."""

    return err(cx, hydra.lib.strings.cat(("Expected ", expected, ", found: ", found)))

def with_type(name: hydra.core.Name, desc: hydra.ext.org.w3.rdf.syntax.Description) -> hydra.ext.org.w3.rdf.syntax.Description:
    r"""Add an rdf:type triple to an RDF Description."""

    subj = desc.subject
    triples = desc.graph.value
    @lru_cache(1)
    def subj_res() -> hydra.ext.org.w3.rdf.syntax.Resource:
        match subj:
            case hydra.ext.org.w3.rdf.syntax.NodeIri(value=iri):
                return cast(hydra.ext.org.w3.rdf.syntax.Resource, hydra.ext.org.w3.rdf.syntax.ResourceIri(iri))

            case hydra.ext.org.w3.rdf.syntax.NodeBnode(value=bnode):
                return cast(hydra.ext.org.w3.rdf.syntax.Resource, hydra.ext.org.w3.rdf.syntax.ResourceBnode(bnode))

            case _:
                raise TypeError("Unsupported Node")
    @lru_cache(1)
    def triple() -> hydra.ext.org.w3.rdf.syntax.Triple:
        return hydra.ext.org.w3.rdf.syntax.Triple(subj_res(), hydra.ext.rdf.utils.rdf_iri("type"), cast(hydra.ext.org.w3.rdf.syntax.Node_, hydra.ext.org.w3.rdf.syntax.NodeIri(hydra.ext.rdf.utils.name_to_iri(name))))
    return hydra.ext.org.w3.rdf.syntax.Description(subj, hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.insert(triple(), triples)))

def encode_field(rname: hydra.core.Name, subject: hydra.ext.org.w3.rdf.syntax.Resource, field: hydra.core.Field, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[frozenlist[hydra.ext.org.w3.rdf.syntax.Triple], hydra.context.Context]]:
    r"""Encode a record field as RDF triples with a given subject."""

    @lru_cache(1)
    def pair1() -> tuple[hydra.ext.org.w3.rdf.syntax.Resource, hydra.context.Context]:
        return hydra.ext.rdf.utils.next_blank_node(cx)
    @lru_cache(1)
    def node() -> hydra.ext.org.w3.rdf.syntax.Resource:
        return hydra.lib.pairs.first(pair1())
    @lru_cache(1)
    def cx1() -> hydra.context.Context:
        return hydra.lib.pairs.second(pair1())
    return hydra.lib.eithers.bind(encode_term(node(), field.term, cx1(), g), (lambda _r1: (descs := hydra.lib.pairs.first(_r1), cx2 := hydra.lib.pairs.second(_r1), Right((hydra.lib.lists.concat2(hydra.ext.rdf.utils.triples_of(descs), hydra.ext.rdf.utils.for_objects(subject, hydra.ext.rdf.utils.property_iri(rname, field.name), hydra.ext.rdf.utils.subjects_of(descs))), cx2)))[2]))

def encode_list(subj: hydra.ext.org.w3.rdf.syntax.Resource, terms: frozenlist[hydra.core.Term], cx0: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[frozenlist[hydra.ext.org.w3.rdf.syntax.Description], hydra.context.Context]]:
    r"""Encode a list of terms as RDF list structure."""

    return hydra.lib.logic.if_else(hydra.lib.lists.null(terms), (lambda : Right(((hydra.ext.org.w3.rdf.syntax.Description(cast(hydra.ext.org.w3.rdf.syntax.Node_, hydra.ext.org.w3.rdf.syntax.NodeIri(hydra.ext.org.w3.rdf.syntax.Iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))), hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.empty())),), cx0))), (lambda : (pair1 := hydra.ext.rdf.utils.next_blank_node(cx0), node1 := hydra.lib.pairs.first(pair1), cx1 := hydra.lib.pairs.second(pair1), hydra.lib.eithers.bind(encode_term(node1, hydra.lib.lists.head(terms), cx1, g), (lambda _r1: (fdescs := hydra.lib.pairs.first(_r1), cx2 := hydra.lib.pairs.second(_r1), first_triples := hydra.lib.lists.concat2(hydra.ext.rdf.utils.triples_of(fdescs), hydra.ext.rdf.utils.for_objects(subj, hydra.ext.rdf.utils.rdf_iri("first"), hydra.ext.rdf.utils.subjects_of(fdescs))), pair2 := hydra.ext.rdf.utils.next_blank_node(cx2), next := hydra.lib.pairs.first(pair2), cx3 := hydra.lib.pairs.second(pair2), hydra.lib.eithers.map((lambda _r2: (rdescs := hydra.lib.pairs.first(_r2), cx4 := hydra.lib.pairs.second(_r2), rest_triples := hydra.lib.lists.concat2(hydra.ext.rdf.utils.triples_of(rdescs), hydra.ext.rdf.utils.for_objects(subj, hydra.ext.rdf.utils.rdf_iri("rest"), hydra.ext.rdf.utils.subjects_of(rdescs))), ((hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.utils.resource_to_node(subj), hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.from_list(hydra.lib.lists.concat2(first_triples, rest_triples)))),), cx4))[3]), encode_list(next, hydra.lib.lists.tail(terms), cx3, g)))[6])))[3]))

def encode_term(subject: hydra.ext.org.w3.rdf.syntax.Resource, term: hydra.core.Term, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[frozenlist[hydra.ext.org.w3.rdf.syntax.Description], hydra.context.Context]]:
    r"""Encode a Hydra term as a list of RDF Descriptions."""

    match term:
        case hydra.core.TermAnnotated(value=at):
            return encode_term(subject, at.body, cx, g)

        case hydra.core.TermList(value=terms):
            return encode_list(subject, terms, cx, g)

        case hydra.core.TermLiteral(value=lit):
            return Right(((hydra.ext.org.w3.rdf.syntax.Description(cast(hydra.ext.org.w3.rdf.syntax.Node_, hydra.ext.org.w3.rdf.syntax.NodeLiteral(hydra.ext.rdf.utils.encode_literal(lit))), hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.empty())),), cx))

        case hydra.core.TermMap(value=m):
            return hydra.lib.eithers.map((lambda _r: ((hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.utils.resource_to_node(subject), hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.pairs.first(_r))))),), hydra.lib.pairs.second(_r))), fold_accum_result((lambda _cx0, kv: hydra.lib.eithers.bind(hydra.extract.core.string(_cx0, g, hydra.strip.deannotate_term(hydra.lib.pairs.first(kv))), (lambda _ks: (pair2 := hydra.ext.rdf.utils.next_blank_node(_cx0), node2 := hydra.lib.pairs.first(pair2), cx2 := hydra.lib.pairs.second(pair2), hydra.lib.eithers.map((lambda _dr: (hydra.lib.lists.concat2(hydra.ext.rdf.utils.for_objects(subject, hydra.ext.rdf.utils.key_iri(_ks), hydra.ext.rdf.utils.subjects_of(hydra.lib.pairs.first(_dr))), hydra.ext.rdf.utils.triples_of(hydra.lib.pairs.first(_dr))), hydra.lib.pairs.second(_dr))), encode_term(node2, hydra.lib.pairs.second(kv), cx2, g)))[3]))), cx, hydra.lib.maps.to_list(m)))

        case hydra.core.TermWrap(value=wt):
            return hydra.lib.eithers.map((lambda _dr: (descs := hydra.lib.pairs.first(_dr), cx1 := hydra.lib.pairs.second(_dr), (hydra.lib.lists.cons(with_type(wt.type_name, hydra.lib.lists.head(descs)), hydra.lib.lists.tail(descs)), cx1))[2]), encode_term(subject, wt.body, cx, g))

        case hydra.core.TermMaybe(value=mterm):
            return hydra.lib.maybes.maybe((lambda : Right(((), cx))), (lambda _inner: encode_term(subject, _inner, cx, g)), mterm)

        case hydra.core.TermRecord(value=rec):
            rname = rec.type_name
            fields = rec.fields
            return hydra.lib.eithers.map((lambda _r: ((with_type(rname, hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.utils.resource_to_node(subject), hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.from_list(hydra.lib.lists.concat(hydra.lib.pairs.first(_r)))))),), hydra.lib.pairs.second(_r))), fold_accum_result((lambda _cx0, field: encode_field(rname, subject, field, _cx0, g)), cx, fields))

        case hydra.core.TermSet(value=terms2):
            return hydra.lib.eithers.map((lambda _r: (hydra.lib.lists.concat(hydra.lib.pairs.first(_r)), hydra.lib.pairs.second(_r))), fold_accum_result((lambda _cx0, t: (pair3 := hydra.ext.rdf.utils.next_blank_node(_cx0), node3 := hydra.lib.pairs.first(pair3), cx3 := hydra.lib.pairs.second(pair3), encode_term(node3, t, cx3, g))[3]), cx, hydra.lib.sets.to_list(terms2)))

        case hydra.core.TermUnion(value=inj):
            rname = inj.type_name
            field = inj.field
            return hydra.lib.eithers.map((lambda _r: ((with_type(rname, hydra.ext.org.w3.rdf.syntax.Description(hydra.ext.rdf.utils.resource_to_node(subject), hydra.ext.org.w3.rdf.syntax.Graph(hydra.lib.sets.from_list(hydra.lib.pairs.first(_r))))),), hydra.lib.pairs.second(_r))), encode_field(rname, subject, field, cx, g))

        case _:
            return unexpected_e(cx, "RDF-compatible term", "unsupported term variant")

def encode_literal_type(lt: hydra.core.LiteralType):
    r"""Encode a LiteralType as SHACL CommonProperties with an XSD datatype constraint."""

    def xsd(local: str) -> hydra.ext.org.w3.shacl.model.CommonProperties:
        return common((cast(hydra.ext.org.w3.shacl.model.CommonConstraint, hydra.ext.org.w3.shacl.model.CommonConstraintDatatype(hydra.ext.rdf.utils.xml_schema_datatype_iri(local))),))
    def _hoist_xsd_body_1(v1):
        match v1:
            case hydra.core.FloatType.BIGFLOAT:
                return xsd("decimal")

            case hydra.core.FloatType.FLOAT32:
                return xsd("float")

            case hydra.core.FloatType.FLOAT64:
                return xsd("double")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_xsd_body_2(v1):
        match v1:
            case hydra.core.IntegerType.BIGINT:
                return xsd("integer")

            case hydra.core.IntegerType.INT8:
                return xsd("byte")

            case hydra.core.IntegerType.INT16:
                return xsd("short")

            case hydra.core.IntegerType.INT32:
                return xsd("int")

            case hydra.core.IntegerType.INT64:
                return xsd("long")

            case hydra.core.IntegerType.UINT8:
                return xsd("unsignedByte")

            case hydra.core.IntegerType.UINT16:
                return xsd("unsignedShort")

            case hydra.core.IntegerType.UINT32:
                return xsd("unsignedInt")

            case hydra.core.IntegerType.UINT64:
                return xsd("unsignedLong")

            case _:
                raise AssertionError("Unreachable: all variants handled")
    match lt:
        case hydra.core.LiteralTypeBinary():
            return xsd("base64Binary")

        case hydra.core.LiteralTypeBoolean():
            return xsd("boolean")

        case hydra.core.LiteralTypeFloat(value=ft):
            return _hoist_xsd_body_1(ft)

        case hydra.core.LiteralTypeInteger(value=it):
            return _hoist_xsd_body_2(it)

        case hydra.core.LiteralTypeString():
            return xsd("string")

        case _:
            raise AssertionError("Unreachable: all variants handled")

def node(constraints: frozenlist[hydra.ext.org.w3.shacl.model.CommonConstraint]) -> hydra.ext.org.w3.shacl.model.Shape:
    r"""Construct a SHACL node shape from a list of common constraints."""

    return cast(hydra.ext.org.w3.shacl.model.Shape, hydra.ext.org.w3.shacl.model.ShapeNode(hydra.ext.org.w3.shacl.model.NodeShape(common(constraints))))

def property(iri: hydra.ext.org.w3.rdf.syntax.Iri) -> hydra.ext.org.w3.shacl.model.PropertyShape:
    r"""Construct a default property shape with the given IRI as its path."""

    return hydra.ext.org.w3.shacl.model.PropertyShape(default_common_properties(), hydra.lib.sets.empty(), Nothing(), hydra.ext.org.w3.rdf.syntax.LangStrings(hydra.lib.maps.empty()), hydra.ext.org.w3.rdf.syntax.LangStrings(hydra.lib.maps.empty()), Nothing(), iri)

def encode_field_type(rname: hydra.core.Name, order: Maybe[int], ft: hydra.core.FieldType, cx: hydra.context.Context) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.org.w3.shacl.model.Definition[hydra.ext.org.w3.shacl.model.PropertyShape]]:
    r"""Encode a FieldType as a SHACL property shape Definition."""

    fname = ft.name
    ftype = ft.type
    @lru_cache(1)
    def iri() -> hydra.ext.org.w3.rdf.syntax.Iri:
        return hydra.ext.rdf.utils.property_iri(rname, fname)
    def for_type(mn: Maybe[int], mx: Maybe[int], t: hydra.core.Type) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.org.w3.shacl.model.Definition[hydra.ext.org.w3.shacl.model.PropertyShape]]:
        while True:
            match hydra.strip.deannotate_type(t):
                case hydra.core.TypeMaybe(value=ot):
                    mn = Just(0)
                    mx = mx
                    t = ot
                    continue

                case hydra.core.TypeSet(value=st):
                    mn = mn
                    mx = Nothing()
                    t = st
                    continue

                case _:
                    return for_type_default(mn, mx, t)
    def for_type_default(mn: Maybe[int], mx: Maybe[int], t: hydra.core.Type) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.org.w3.shacl.model.Definition[hydra.ext.org.w3.shacl.model.PropertyShape]]:
        return hydra.lib.eithers.map((lambda _cp: (base_prop := property(iri()), min_c := hydra.lib.maybes.map((lambda _n: cast(hydra.ext.org.w3.shacl.model.PropertyShapeConstraint, hydra.ext.org.w3.shacl.model.PropertyShapeConstraintMinCount(_n))), mn), max_c := hydra.lib.maybes.map((lambda _n: cast(hydra.ext.org.w3.shacl.model.PropertyShapeConstraint, hydra.ext.org.w3.shacl.model.PropertyShapeConstraintMaxCount(_n))), mx), hydra.ext.org.w3.shacl.model.Definition(iri(), hydra.ext.org.w3.shacl.model.PropertyShape(_cp, hydra.lib.sets.from_list(hydra.lib.maybes.cat((min_c, max_c))), Nothing(), hydra.ext.org.w3.rdf.syntax.LangStrings(hydra.lib.maps.empty()), hydra.ext.org.w3.rdf.syntax.LangStrings(hydra.lib.maps.empty()), order, iri())))[3]), encode_type(rname, t, cx))
    return for_type(Just(1), Just(1), ftype)

def encode_type(tname: hydra.core.Name, typ: hydra.core.Type, cx: hydra.context.Context) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.org.w3.shacl.model.CommonProperties]:
    r"""Encode a Hydra type as SHACL CommonProperties."""

    @lru_cache(1)
    def any() -> Either[T0, hydra.ext.org.w3.shacl.model.CommonProperties]:
        return Right(common(()))
    match hydra.strip.deannotate_type(typ):
        case hydra.core.TypeEither():
            return any()

        case hydra.core.TypeList():
            return any()

        case hydra.core.TypeLiteral(value=lt):
            return Right(encode_literal_type(lt))

        case hydra.core.TypeMap():
            return any()

        case hydra.core.TypePair():
            return any()

        case hydra.core.TypeWrap():
            return any()

        case hydra.core.TypeRecord(value=fts):
            return hydra.lib.eithers.map((lambda _props: common((cast(hydra.ext.org.w3.shacl.model.CommonConstraint, hydra.ext.org.w3.shacl.model.CommonConstraintProperty(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda _p: cast(hydra.ext.org.w3.shacl.model.Reference, hydra.ext.org.w3.shacl.model.ReferenceDefinition(_p))), _props)))),))), hydra.lib.eithers.map_list((lambda _pair: encode_field_type(tname, Just(hydra.lib.pairs.first(_pair)), hydra.lib.pairs.second(_pair), cx)), hydra.lib.lists.zip(hydra.lib.lists.map((lambda _i: hydra.lib.literals.int32_to_bigint(_i)), hydra.lib.math.range_(0, hydra.lib.lists.length(fts))), fts)))

        case hydra.core.TypeSet():
            return any()

        case hydra.core.TypeUnion(value=fts2):
            return hydra.lib.eithers.map((lambda _props: common((cast(hydra.ext.org.w3.shacl.model.CommonConstraint, hydra.ext.org.w3.shacl.model.CommonConstraintXone(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda _p: cast(hydra.ext.org.w3.shacl.model.Reference, hydra.ext.org.w3.shacl.model.ReferenceAnonymous(node((cast(hydra.ext.org.w3.shacl.model.CommonConstraint, hydra.ext.org.w3.shacl.model.CommonConstraintProperty(hydra.lib.sets.from_list((cast(hydra.ext.org.w3.shacl.model.Reference, hydra.ext.org.w3.shacl.model.ReferenceDefinition(_p)),)))),))))), _props)))),))), hydra.lib.eithers.map_list((lambda _ft: encode_field_type(tname, Nothing(), _ft, cx)), fts2))

        case hydra.core.TypeUnit():
            return any()

        case hydra.core.TypeVariable(value=vname):
            return Right(common((cast(hydra.ext.org.w3.shacl.model.CommonConstraint, hydra.ext.org.w3.shacl.model.CommonConstraintNode(hydra.lib.sets.from_list((cast(hydra.ext.org.w3.shacl.model.Reference, hydra.ext.org.w3.shacl.model.ReferenceNamed(hydra.ext.rdf.utils.name_to_iri(vname))),)))),)))

        case _:
            return unexpected_e(cx, "type", "unsupported type variant")

def shacl_coder(mod: hydra.module.Module, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.errors.Error], tuple[hydra.ext.org.w3.shacl.model.ShapesGraph, hydra.context.Context]]:
    r"""Encode a module's type elements as a SHACL ShapesGraph."""

    @lru_cache(1)
    def type_els():
        def _hoist_type_els_1(v1):
            match v1:
                case hydra.module.DefinitionType(value=td):
                    return Just(hydra.annotations.type_element(td.name, td.type))

                case _:
                    return Nothing()
        return hydra.lib.maybes.cat(hydra.lib.lists.map((lambda d: _hoist_type_els_1(d)), mod.definitions))
    def to_shape(el: hydra.core.Binding) -> Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.org.w3.shacl.model.Definition[hydra.ext.org.w3.shacl.model.Shape]]:
        return hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda _de: hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError(_de.value))), cx)), (lambda _t: _t), hydra.decode.core.type(g, el.term)), (lambda _typ: hydra.lib.eithers.map((lambda _cp: hydra.ext.org.w3.shacl.model.Definition(element_iri(el), cast(hydra.ext.org.w3.shacl.model.Shape, hydra.ext.org.w3.shacl.model.ShapeNode(hydra.ext.org.w3.shacl.model.NodeShape(_cp))))), encode_type(el.name, _typ, cx))))
    return hydra.lib.eithers.map((lambda _shapes: (hydra.ext.org.w3.shacl.model.ShapesGraph(hydra.lib.sets.from_list(_shapes)), cx)), hydra.lib.eithers.map_list((lambda x1: to_shape(x1)), type_els()))
