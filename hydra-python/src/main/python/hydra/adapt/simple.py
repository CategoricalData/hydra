# Note: this is an automatically generated file. Do not edit.

r"""Simple, one-way adapters for types and terms."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.compute
import hydra.core
import hydra.graph
import hydra.inference
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.literals
import hydra.module
import hydra.reduction
import hydra.reflect
import hydra.rewriting
import hydra.schemas
import hydra.show.core

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def literal_type_supported(constraints: hydra.coders.LanguageConstraints, lt: hydra.core.LiteralType) -> bool:
    r"""Check if a literal type is supported by the given language constraints."""
    
    def for_type(lt2: hydra.core.LiteralType) -> bool:
        match lt2:
            case hydra.core.LiteralTypeFloat(value=ft):
                return hydra.lib.sets.member(ft, constraints.float_types)
            
            case hydra.core.LiteralTypeInteger(value=it):
                return hydra.lib.sets.member(it, constraints.integer_types)
            
            case _:
                return True
    return hydra.lib.logic.if_else(hydra.lib.sets.member(hydra.reflect.literal_type_variant(lt), constraints.literal_variants), (lambda : for_type(lt)), (lambda : False))

def type_alternatives(type: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    r"""Find a list of alternatives for a given type, if any."""
    
    match type:
        case hydra.core.TypeAnnotated(value=at):
            type2 = at.body
            return (type2,)
        
        case hydra.core.TypeMaybe(value=ot):
            return (cast(hydra.core.Type, hydra.core.TypeList(ot)),)
        
        case hydra.core.TypeUnion(value=rt):
            tname = rt.type_name
            fields = rt.fields
            def to_opt_field(f: hydra.core.FieldType) -> hydra.core.Type:
                return hydra.core.FieldType(f.name, cast(hydra.core.Type, hydra.core.TypeMaybe(f.type)))
            def opt_fields() -> frozenlist[hydra.core.FieldType]:
                return hydra.lib.lists.map(to_opt_field, fields)
            return (cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(tname, opt_fields()))),)
        
        case hydra.core.TypeUnit():
            return (cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean()))),)
        
        case _:
            return cast(frozenlist[hydra.core.Type], ())

def adapt_type(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], type0: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    def for_supported(typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
        match typ:
            case hydra.core.TypeLiteral(value=lt):
                return hydra.lib.logic.if_else(literal_type_supported(constraints, lt), (lambda : cast(Maybe[hydra.core.Type], Just(typ))), (lambda : hydra.lib.maybes.maybe(cast(Maybe[hydra.core.Type], Just(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()))))), (lambda lt2: cast(Maybe[hydra.core.Type], Just(cast(hydra.core.Type, hydra.core.TypeLiteral(lt2))))), hydra.lib.maps.lookup(lt, litmap))))
            
            case _:
                return cast(Maybe[hydra.core.Type], Just(typ))
    def for_unsupported(typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
        def try_alts(alts: frozenlist[hydra.core.Type]) -> Maybe[hydra.core.Type]:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(alts), (lambda : cast(Maybe[hydra.core.Type], Nothing())), (lambda : hydra.lib.maybes.maybe(try_alts(hydra.lib.lists.tail(alts)), (lambda t: cast(Maybe[hydra.core.Type], Just(t))), try_type(hydra.lib.lists.head(alts)))))
        def alts() -> frozenlist[hydra.core.Type]:
            return type_alternatives(typ)
        return try_alts(alts())
    def try_type(typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
        def supported_variant() -> bool:
            return hydra.lib.sets.member(hydra.reflect.type_variant(typ), constraints.type_variants)
        return hydra.lib.logic.if_else(supported_variant(), (lambda : for_supported(typ)), (lambda : for_unsupported(typ)))
    def rewrite(recurse: Callable[[hydra.core.Type], hydra.compute.Flow[T1, hydra.core.Type]], typ: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.core.Type]:
        return hydra.lib.flows.bind(recurse(typ), (lambda type1: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("no alternatives for type: ", hydra.show.core.type(typ))), (lambda type2: hydra.lib.flows.pure(type2)), try_type(type1))))
    return hydra.rewriting.rewrite_type_m(cast(Callable[[
      Callable[[hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]],
      hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]], (lambda x1, x2: rewrite(x1, x2))), type0)

def adapt_graph_schema(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], types0: FrozenDict[T0, hydra.core.Type]) -> hydra.compute.Flow[T1, FrozenDict[T0, hydra.core.Type]]:
    def map_pair(pair: tuple[T2, hydra.core.Type]) -> hydra.compute.Flow[T3, tuple[T2, hydra.core.Type]]:
        def name() -> T2:
            return hydra.lib.pairs.first(pair)
        def typ() -> hydra.core.Type:
            return hydra.lib.pairs.second(pair)
        return hydra.lib.flows.bind(adapt_type(constraints, litmap, typ()), (lambda typ1: hydra.lib.flows.pure(cast(tuple[T2, hydra.core.Type], (name(), typ1)))))
    return hydra.lib.flows.bind(hydra.lib.flows.map_list(cast(Callable[[tuple[T0, hydra.core.Type]], hydra.compute.Flow[T1, tuple[T0, hydra.core.Type]]], (lambda x1: map_pair(x1))), hydra.lib.maps.to_list(types0)), (lambda pairs: hydra.lib.flows.pure(cast(FrozenDict[T0, hydra.core.Type], hydra.lib.maps.from_list(pairs)))))

def adapt_float_type(constraints: hydra.coders.LanguageConstraints, ft: hydra.core.FloatType) -> Maybe[hydra.core.FloatType]:
    r"""Attempt to adapt a floating-point type using the given language constraints."""
    
    def supported() -> bool:
        return hydra.lib.sets.member(ft, constraints.float_types)
    def alt(v1: hydra.core.FloatType) -> Maybe[hydra.core.FloatType]:
        return adapt_float_type(constraints, v1)
    def for_unsupported(ft2: hydra.core.FloatType) -> Maybe[hydra.core.FloatType]:
        match ft2:
            case hydra.core.FloatType.BIGFLOAT:
                return alt(hydra.core.FloatType.FLOAT64)
            
            case hydra.core.FloatType.FLOAT32:
                return alt(hydra.core.FloatType.FLOAT64)
            
            case hydra.core.FloatType.FLOAT64:
                return alt(hydra.core.FloatType.BIGFLOAT)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.logic.if_else(supported(), (lambda : cast(Maybe[hydra.core.FloatType], Just(ft))), (lambda : for_unsupported(ft)))

def adapt_integer_type(constraints: hydra.coders.LanguageConstraints, it: hydra.core.IntegerType) -> Maybe[hydra.core.IntegerType]:
    r"""Attempt to adapt an integer type using the given language constraints."""
    
    def supported() -> bool:
        return hydra.lib.sets.member(it, constraints.integer_types)
    def alt(v1: hydra.core.IntegerType) -> Maybe[hydra.core.IntegerType]:
        return adapt_integer_type(constraints, v1)
    def for_unsupported(it2: hydra.core.IntegerType) -> Maybe[hydra.core.IntegerType]:
        match it2:
            case hydra.core.IntegerType.BIGINT:
                return cast(Maybe[hydra.core.IntegerType], Nothing())
            
            case hydra.core.IntegerType.INT8:
                return alt(hydra.core.IntegerType.UINT16)
            
            case hydra.core.IntegerType.INT16:
                return alt(hydra.core.IntegerType.UINT32)
            
            case hydra.core.IntegerType.INT32:
                return alt(hydra.core.IntegerType.UINT64)
            
            case hydra.core.IntegerType.INT64:
                return alt(hydra.core.IntegerType.BIGINT)
            
            case hydra.core.IntegerType.UINT8:
                return alt(hydra.core.IntegerType.INT16)
            
            case hydra.core.IntegerType.UINT16:
                return alt(hydra.core.IntegerType.INT32)
            
            case hydra.core.IntegerType.UINT32:
                return alt(hydra.core.IntegerType.INT64)
            
            case hydra.core.IntegerType.UINT64:
                return alt(hydra.core.IntegerType.BIGINT)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.logic.if_else(supported(), (lambda : cast(Maybe[hydra.core.IntegerType], Just(it))), (lambda : for_unsupported(it)))

def adapt_literal_type(constraints: hydra.coders.LanguageConstraints, lt: hydra.core.LiteralType) -> Maybe[hydra.core.LiteralType]:
    r"""Attempt to adapt a literal type using the given language constraints."""
    
    def for_unsupported(lt2: hydra.core.LiteralType) -> Maybe[hydra.core.LiteralType]:
        match lt2:
            case hydra.core.LiteralTypeBinary():
                return cast(Maybe[hydra.core.LiteralType], Just(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString())))
            
            case hydra.core.LiteralTypeBoolean():
                return hydra.lib.maybes.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), adapt_integer_type(constraints, hydra.core.IntegerType.INT8))
            
            case hydra.core.LiteralTypeFloat(value=ft):
                return hydra.lib.maybes.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(x))), adapt_float_type(constraints, ft))
            
            case hydra.core.LiteralTypeInteger(value=it):
                return hydra.lib.maybes.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), adapt_integer_type(constraints, it))
            
            case _:
                return cast(Maybe[hydra.core.LiteralType], Nothing())
    return hydra.lib.logic.if_else(literal_type_supported(constraints, lt), (lambda : cast(Maybe[hydra.core.LiteralType], Nothing())), (lambda : for_unsupported(lt)))

def adapt_literal_types_map(constraints: hydra.coders.LanguageConstraints) -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
    r"""Derive a map of adapted literal types for the given language constraints."""
    
    def try_type(lt: hydra.core.LiteralType) -> Maybe[tuple[hydra.core.LiteralType, hydra.core.LiteralType]]:
        return hydra.lib.maybes.maybe(cast(Maybe[tuple[hydra.core.LiteralType, hydra.core.LiteralType]], Nothing()), (lambda lt2: cast(Maybe[tuple[hydra.core.LiteralType, hydra.core.LiteralType]], Just(cast(tuple[hydra.core.LiteralType, hydra.core.LiteralType], (lt, lt2))))), adapt_literal_type(constraints, lt))
    return cast(FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map(try_type, hydra.reflect.literal_types()))))

def adapt_type_scheme(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], ts0: hydra.core.TypeScheme) -> hydra.compute.Flow[T0, hydra.core.TypeScheme]:
    vars0 = ts0.variables
    t0 = ts0.type
    return hydra.lib.flows.bind(adapt_type(constraints, litmap, t0), (lambda t1: hydra.lib.flows.pure(hydra.core.TypeScheme(vars0, t1, ts0.constraints))))

def adapt_primitive(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], prim0: hydra.graph.Primitive) -> hydra.compute.Flow[T0, hydra.graph.Primitive]:
    ts0 = prim0.type
    return hydra.lib.flows.bind(adapt_type_scheme(constraints, litmap, ts0), (lambda ts1: hydra.lib.flows.pure(hydra.graph.Primitive(prim0.name, ts1, prim0.implementation))))

def adapt_literal(lt: hydra.core.LiteralType, l: hydra.core.Literal) -> hydra.core.Type:
    def _hoist_hydra_adapt_simple_adapt_literal_1(b: bytes, v1: hydra.core.LiteralType) -> hydra.core.Type:
        match v1:
            case hydra.core.LiteralTypeString():
                return cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.binary_to_string(b)))
            
            case _:
                raise TypeError("Unsupported LiteralType")
    def _hoist_hydra_adapt_simple_adapt_literal_2(b: bool, v1: hydra.core.LiteralType) -> hydra.core.Type:
        match v1:
            case hydra.core.LiteralTypeInteger(value=it):
                return cast(hydra.core.Literal, hydra.core.LiteralInteger(hydra.literals.bigint_to_integer_value(it, hydra.lib.logic.if_else(b, (lambda : 1), (lambda : 0)))))
            
            case _:
                raise TypeError("Unsupported LiteralType")
    def _hoist_hydra_adapt_simple_adapt_literal_3(f: hydra.core.FloatValue, v1: hydra.core.LiteralType) -> hydra.core.Type:
        match v1:
            case hydra.core.LiteralTypeFloat(value=ft):
                return cast(hydra.core.Literal, hydra.core.LiteralFloat(hydra.literals.bigfloat_to_float_value(ft, hydra.literals.float_value_to_bigfloat(f))))
            
            case _:
                raise TypeError("Unsupported LiteralType")
    def _hoist_hydra_adapt_simple_adapt_literal_4(i: hydra.core.IntegerValue, v1: hydra.core.LiteralType) -> hydra.core.Type:
        match v1:
            case hydra.core.LiteralTypeInteger(value=it):
                return cast(hydra.core.Literal, hydra.core.LiteralInteger(hydra.literals.bigint_to_integer_value(it, hydra.literals.integer_value_to_bigint(i))))
            
            case _:
                raise TypeError("Unsupported LiteralType")
    match l:
        case hydra.core.LiteralBinary(value=b):
            return _hoist_hydra_adapt_simple_adapt_literal_1(b, lt)
        
        case hydra.core.LiteralBoolean(value=b2):
            return _hoist_hydra_adapt_simple_adapt_literal_2(b2, lt)
        
        case hydra.core.LiteralFloat(value=f):
            return _hoist_hydra_adapt_simple_adapt_literal_3(f, lt)
        
        case hydra.core.LiteralInteger(value=i):
            return _hoist_hydra_adapt_simple_adapt_literal_4(i, lt)
        
        case _:
            raise TypeError("Unsupported Literal")

def adapt_literal_value(litmap: FrozenDict[T0, hydra.core.LiteralType], lt: T0, l: hydra.core.Literal) -> hydra.core.Type:
    return hydra.lib.maybes.maybe(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.show.core.literal(l))), (lambda lt2: adapt_literal(lt2, l)), hydra.lib.maps.lookup(lt, litmap))

def term_alternatives(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Term]]:
    r"""Find a list of alternatives for a given term, if any."""
    
    match term:
        case hydra.core.TermAnnotated(value=at):
            term2 = at.body
            return hydra.lib.flows.pure((term2,))
        
        case hydra.core.TermMaybe(value=ot):
            return hydra.lib.flows.pure((cast(hydra.core.Term, hydra.core.TermList(hydra.lib.maybes.maybe(cast(frozenlist[hydra.core.Term], ()), (lambda term2: (term2,)), ot))),))
        
        case hydra.core.TermUnion(value=inj):
            tname = inj.type_name
            field = inj.field
            fname = field.name
            fterm = field.term
            def for_field_type(ft: hydra.core.FieldType) -> hydra.core.Type:
                ftname = ft.name
                return hydra.core.Field(fname, cast(hydra.core.Term, hydra.core.TermMaybe(hydra.lib.logic.if_else(hydra.lib.equality.equal(ftname, fname), (lambda : cast(Maybe[hydra.core.Term], Just(fterm))), (lambda : cast(Maybe[hydra.core.Term], Nothing()))))))
            return hydra.lib.flows.bind(hydra.schemas.require_union_type(tname), (lambda rt: hydra.lib.flows.pure((cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(tname, hydra.lib.lists.map(for_field_type, rt.fields)))),))))
        
        case hydra.core.TermUnit():
            return hydra.lib.flows.pure((cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))),))
        
        case hydra.core.TermWrap(value=wt):
            term2 = wt.body
            return hydra.lib.flows.pure((term2,))
        
        case _:
            return hydra.lib.flows.pure(cast(frozenlist[hydra.core.Term], ()))

def adapt_term(constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], term0: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
    r"""Adapt a term using the given language constraints."""
    
    def rewrite(recurse: Callable[[T0], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], term02: T0) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        def for_supported(term: hydra.core.Term) -> hydra.compute.Flow[T1, Maybe[hydra.core.Term]]:
            match term:
                case hydra.core.TermLiteral(value=l):
                    lt = hydra.reflect.literal_type(l)
                    return hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Just(hydra.lib.logic.if_else(literal_type_supported(constraints, lt), (lambda : term), (lambda : cast(hydra.core.Term, hydra.core.TermLiteral(adapt_literal_value(litmap, lt, l))))))))
                
                case _:
                    return hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Just(term)))
        def for_unsupported(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
            def for_non_null(alts: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
                return hydra.lib.flows.bind(try_term(hydra.lib.lists.head(alts)), (lambda mterm: hydra.lib.maybes.maybe(try_alts(hydra.lib.lists.tail(alts)), (lambda t: hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Just(t)))), mterm)))
            def try_alts(alts: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(alts), (lambda : hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Nothing()))), (lambda : for_non_null(alts)))
            return hydra.lib.flows.bind(term_alternatives(term), (lambda alts: try_alts(alts)))
        def try_term(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
            def supported_variant() -> bool:
                return hydra.lib.sets.member(hydra.reflect.term_variant(term), constraints.term_variants)
            return hydra.lib.logic.if_else(supported_variant(), (lambda : for_supported(term)), (lambda : for_unsupported(term)))
        return hydra.lib.flows.bind(recurse(term02), (lambda term1: hydra.lib.flows.bind(try_term(term1), (lambda mterm: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("no alternatives for term: ", hydra.show.core.term(term1))), (lambda term2: hydra.lib.flows.pure(term2)), mterm)))))
    return hydra.rewriting.rewrite_term_m(cast(Callable[[
      Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]],
      hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], (lambda x1, x2: rewrite(x1, x2))), term0)

def adapt_data_graph(constraints: hydra.coders.LanguageConstraints, do_expand: bool, graph0: hydra.graph.Graph) -> hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph]:
    r"""Adapt a graph and its schema to the given language constraints. The doExpand flag controls eta expansion of partial applications. Note: case statement hoisting is done separately, prior to inference."""
    
    def transform(graph: hydra.graph.Graph, gterm: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        return hydra.lib.flows.bind(hydra.schemas.graph_to_type_context(graph), (lambda tx: (gterm1 := hydra.rewriting.unshadow_variables(gterm), hydra.lib.flows.bind(hydra.lib.logic.if_else(do_expand, (lambda : hydra.reduction.eta_expand_typed_term(tx, gterm1)), (lambda : hydra.lib.flows.pure(gterm1))), (lambda gterm2: (gterm3 := hydra.rewriting.remove_types_from_term(gterm2), hydra.lib.flows.pure(hydra.rewriting.lift_lambda_above_let(gterm3)))[1])))[1]))
    def litmap() -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
        return adapt_literal_types_map(constraints)
    els0 = graph0.elements
    env0 = graph0.environment
    body0 = graph0.body
    prims0 = graph0.primitives
    schema0 = graph0.schema
    return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.graph.Graph], Nothing())), (lambda sg: hydra.lib.flows.bind(hydra.schemas.graph_as_types(sg), (lambda tmap0: hydra.lib.flows.bind(adapt_graph_schema(constraints, litmap(), tmap0), (lambda tmap1: (emap := hydra.schemas.types_to_elements(tmap1), hydra.lib.flows.pure(cast(Maybe[hydra.graph.Graph], Just(hydra.graph.Graph(emap, sg.environment, sg.types, sg.body, sg.primitives, sg.schema)))))[1]))))), schema0), (lambda schema1: (gterm0 := hydra.schemas.graph_as_term(graph0), hydra.lib.flows.bind(hydra.lib.logic.if_else(do_expand, (lambda : transform(graph0, gterm0)), (lambda : hydra.lib.flows.pure(gterm0))), (lambda gterm1: hydra.lib.flows.bind(adapt_term(constraints, litmap(), gterm1), (lambda gterm2: (els1_raw := hydra.schemas.term_as_graph(gterm2), hydra.lib.flows.bind(hydra.lib.flows.map_elems((lambda v1: adapt_primitive(constraints, litmap(), v1)), prims0), (lambda prims1: (original_constraints := (lambda : cast(FrozenDict[hydra.core.Name, FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]], hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map((lambda el: hydra.lib.maybes.bind(el.type, (lambda ts: hydra.lib.maybes.map((lambda c: cast(tuple[hydra.core.Name, FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]], (el.name, c))), ts.constraints)))), hydra.lib.maps.elems(els0)))))), merge_constraints := (lambda el: (bname := el.name, orig_constraints := (lambda : hydra.lib.maps.lookup(bname, original_constraints())), hydra.lib.maybes.maybe(el, (lambda orig_c: hydra.lib.maybes.maybe(hydra.core.Binding(bname, el.term, cast(Maybe[hydra.core.TypeScheme], Just(hydra.core.TypeScheme(cast(frozenlist[hydra.core.Name], ()), cast(hydra.core.Type, hydra.core.TypeVariable(hydra.core.Name("a"))), cast(Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]], Just(orig_c)))))), (lambda ts: (inferred_c := ts.constraints, merged_c := (lambda : hydra.lib.maybes.maybe(cast(Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]], Just(orig_c)), (lambda inf_c: cast(Maybe[FrozenDict[hydra.core.Name, hydra.core.TypeVariableMetadata]], Just(hydra.lib.maps.union(orig_c, inf_c)))), inferred_c)), hydra.core.Binding(bname, el.term, cast(Maybe[hydra.core.TypeScheme], Just(hydra.core.TypeScheme(ts.variables, ts.type, merged_c())))))[2]), el.type)), orig_constraints()))[2]), els1 := (lambda : cast(FrozenDict[hydra.core.Name, hydra.core.Binding], hydra.lib.maps.from_list(hydra.lib.lists.map((lambda el: cast(tuple[hydra.core.Name, hydra.core.Binding], (el.name, merge_constraints(el)))), hydra.lib.maps.elems(els1_raw))))), hydra.lib.flows.pure(hydra.graph.Graph(els1(), env0, cast(FrozenDict[hydra.core.Name, hydra.core.TypeScheme], hydra.lib.maps.empty()), cast(hydra.core.Term, hydra.core.TermUnit()), prims1, schema1)))[3])))[1])))))[1]))

def data_graph_to_definitions(constraints: hydra.coders.LanguageConstraints, do_expand: bool, do_hoist: bool, graph: hydra.graph.Graph, name_lists: frozenlist[frozenlist[hydra.core.Name]]) -> hydra.compute.Flow[hydra.graph.Graph, tuple[hydra.graph.Graph, frozenlist[frozenlist[hydra.module.TermDefinition]]]]:
    r"""Given a data graph along with language constraints and a designated list of element names, adapt the graph to the language constraints, perform inference, then return a corresponding term definition for each element name. The doExpand flag controls eta expansion; doHoist controls case statement hoisting."""
    
    return hydra.lib.flows.bind(hydra.lib.logic.if_else(do_hoist, (lambda : "let terms are not supported here"), (lambda : hydra.lib.flows.pure(graph))), (lambda graphu0: hydra.lib.flows.bind(hydra.lib.logic.if_else(do_hoist, (lambda : hydra.reduction.hoist_case_statements_in_graph(graphu0)), (lambda : hydra.lib.flows.pure(graphu0))), (lambda graphh: hydra.lib.flows.bind(hydra.lib.logic.if_else(do_hoist, (lambda : "let terms are not supported here"), (lambda : hydra.lib.flows.pure(graphh))), (lambda graphu: hydra.lib.flows.bind(hydra.lib.logic.if_else(do_expand, (lambda : hydra.inference.infer_graph_types(graphu)), (lambda : hydra.lib.flows.pure(graphu))), (lambda graphi: hydra.lib.flows.bind(adapt_data_graph(constraints, do_expand, graphi), (lambda graph1: hydra.lib.flows.bind(hydra.inference.infer_graph_types(graph1), (lambda graph2: (to_def := (lambda el: (ts := (lambda : hydra.lib.maybes.from_just(el.type)), hydra.module.TermDefinition(el.name, el.term, ts()))[1]), hydra.lib.flows.pure(cast(tuple[hydra.graph.Graph, frozenlist[frozenlist[hydra.module.TermDefinition]]], (graph2, hydra.lib.lists.map((lambda names: hydra.lib.lists.map(to_def, hydra.lib.lists.map((lambda n: hydra.lib.maybes.from_just(hydra.lib.maps.lookup(n, graph2.elements))), names))), name_lists)))))[1]))))))))))))

def schema_graph_to_definitions(constraints: hydra.coders.LanguageConstraints, graph: hydra.graph.Graph, name_lists: frozenlist[frozenlist[hydra.core.Name]]) -> hydra.compute.Flow[hydra.graph.Graph, tuple[FrozenDict[hydra.core.Name, hydra.core.Type], frozenlist[frozenlist[hydra.module.TypeDefinition]]]]:
    r"""Given a schema graph along with language constraints and a designated list of element names, adapt the graph to the language constraints, then return a corresponding type definition for each element name."""
    
    def litmap() -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
        return adapt_literal_types_map(constraints)
    return hydra.lib.flows.bind(hydra.schemas.graph_as_types(graph), (lambda tmap0: hydra.lib.flows.bind(adapt_graph_schema(constraints, litmap(), tmap0), (lambda tmap1: (to_def := (lambda pair: hydra.module.TypeDefinition(hydra.lib.pairs.first(pair), hydra.lib.pairs.second(pair))), hydra.lib.flows.pure(cast(tuple[FrozenDict[hydra.core.Name, hydra.core.Type], frozenlist[frozenlist[hydra.module.TypeDefinition]]], (tmap1, hydra.lib.lists.map((lambda names: hydra.lib.lists.map(to_def, hydra.lib.lists.map((lambda n: cast(tuple[hydra.core.Name, hydra.core.Type], (n, hydra.lib.maybes.from_just(hydra.lib.maps.lookup(n, tmap1))))), names))), name_lists)))))[1]))))
