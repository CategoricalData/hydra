# Note: this is an automatically generated file. Do not edit.

r"""Simple, one-way adapters for types and terms."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import Tuple, cast
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
import hydra.lib.sets
import hydra.lib.strings
import hydra.literals
import hydra.module
import hydra.reduction
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.variants

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
    return hydra.lib.logic.if_else(hydra.lib.sets.member(hydra.variants.literal_type_variant(lt), constraints.literal_variants), for_type(lt), False)

def type_alternatives(type: hydra.core.Type) -> frozenlist[hydra.core.Type]:
    r"""Find a list of alternatives for a given type, if any."""
    
    match type:
        case hydra.core.TypeAnnotated(value=at):
            type2 = at.body
            return (type2,)
        
        case hydra.core.TypeOptional(value=ot):
            return (cast(hydra.core.Type, hydra.core.TypeList(ot)),)
        
        case hydra.core.TypeUnion(value=rt):
            tname = rt.type_name
            fields = rt.fields
            return (cast(hydra.core.Type, hydra.core.TypeRecord(hydra.core.RowType(tname, fields))),)
        
        case hydra.core.TypeUnit():
            return (cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeBoolean(None)))),)
        
        case _:
            return cast(frozenlist[hydra.core.Type], ())

def adapt_type[T0](constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], type0: hydra.core.Type) -> hydra.compute.Flow[T0, hydra.core.Type]:
    def for_supported(typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
        match typ:
            case hydra.core.TypeLiteral(value=lt):
                return hydra.lib.logic.if_else(literal_type_supported(constraints, lt), cast(Maybe[hydra.core.Type], Just(typ)), hydra.lib.maybes.maybe(cast(Maybe[hydra.core.Type], Just(cast(hydra.core.Type, hydra.core.TypeLiteral(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString(None)))))), (lambda lt2: cast(Maybe[hydra.core.Type], Just(cast(hydra.core.Type, hydra.core.TypeLiteral(lt2))))), hydra.lib.maps.lookup(lt, litmap)))
            
            case _:
                return cast(Maybe[hydra.core.Type], Just(typ))
    def for_unsupported(typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
        def try_alts(alts: frozenlist[hydra.core.Type]) -> Maybe[hydra.core.Type]:
            return hydra.lib.logic.if_else(hydra.lib.lists.null(alts), cast(Maybe[hydra.core.Type], Nothing()), hydra.lib.maybes.maybe(try_alts(hydra.lib.lists.tail(alts)), (lambda t: cast(Maybe[hydra.core.Type], Just(t))), try_type(hydra.lib.lists.head(alts))))
        alts = type_alternatives(typ)
        return try_alts(alts)
    def try_type(typ: hydra.core.Type) -> Maybe[hydra.core.Type]:
        supported_variant = hydra.lib.sets.member(hydra.variants.type_variant(typ), constraints.type_variants)
        return hydra.lib.logic.if_else(supported_variant, for_supported(typ), for_unsupported(typ))
    def rewrite[T1](recurse: Callable[[hydra.core.Type], hydra.compute.Flow[T1, hydra.core.Type]], typ: hydra.core.Type) -> hydra.compute.Flow[T1, hydra.core.Type]:
        return hydra.lib.flows.bind(recurse(typ), (lambda type1: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("no alternatives for type: ", hydra.show.core.type(typ)))), (lambda type2: hydra.lib.flows.pure(type2)), try_type(type1))))
    return hydra.rewriting.rewrite_type_m(cast(Callable[[
      Callable[[hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]],
      hydra.core.Type], hydra.compute.Flow[T0, hydra.core.Type]], rewrite), type0)

def adapt_graph_schema[T0, T1](constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], types0: FrozenDict[T0, hydra.core.Type]) -> hydra.compute.Flow[T1, FrozenDict[T0, hydra.core.Type]]:
    def map_pair[T2, T3](pair: Tuple[T2, hydra.core.Type]) -> hydra.compute.Flow[T3, Tuple[T2, hydra.core.Type]]:
        name = pair[0]
        typ = pair[1]
        return hydra.lib.flows.bind(adapt_type(constraints, litmap, typ), (lambda typ1: hydra.lib.flows.pure((name, typ1))))
    return hydra.lib.flows.bind(hydra.lib.flows.map_list(cast(Callable[[Tuple[T0, hydra.core.Type]], hydra.compute.Flow[T1, Tuple[T0, hydra.core.Type]]], map_pair), hydra.lib.maps.to_list(types0)), (lambda pairs: hydra.lib.flows.pure(cast(FrozenDict[T0, hydra.core.Type], hydra.lib.maps.from_list(pairs)))))

def adapt_float_type(constraints: hydra.coders.LanguageConstraints, ft: hydra.core.FloatType) -> Maybe[hydra.core.FloatType]:
    r"""Attempt to adapt a floating-point type using the given language constraints."""
    
    supported = hydra.lib.sets.member(ft, constraints.float_types)
    def alt(v1: hydra.core.FloatType) -> Maybe[hydra.core.FloatType]:
        return adapt_float_type(constraints, v1)
    def for_unsupported(ft2: hydra.core.FloatType) -> Maybe[hydra.core.FloatType]:
        match ft2:
            case hydra.core.FloatType.BIGFLOAT:
                return cast(Maybe[hydra.core.FloatType], Nothing())
            
            case hydra.core.FloatType.FLOAT32:
                return alt(hydra.core.FloatType.FLOAT64)
            
            case hydra.core.FloatType.FLOAT64:
                return alt(hydra.core.FloatType.BIGFLOAT)
    return hydra.lib.logic.if_else(supported, cast(Maybe[hydra.core.FloatType], Just(ft)), for_unsupported(ft))

def adapt_integer_type(constraints: hydra.coders.LanguageConstraints, it: hydra.core.IntegerType) -> Maybe[hydra.core.IntegerType]:
    r"""Attempt to adapt an integer type using the given language constraints."""
    
    supported = hydra.lib.sets.member(it, constraints.integer_types)
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
    return hydra.lib.logic.if_else(supported, cast(Maybe[hydra.core.IntegerType], Just(it)), for_unsupported(it))

def adapt_literal_type(constraints: hydra.coders.LanguageConstraints, lt: hydra.core.LiteralType) -> Maybe[hydra.core.LiteralType]:
    r"""Attempt to adapt a literal type using the given language constraints."""
    
    def for_unsupported(lt2: hydra.core.LiteralType) -> Maybe[hydra.core.LiteralType]:
        match lt2:
            case hydra.core.LiteralTypeBinary():
                return cast(Maybe[hydra.core.LiteralType], Just(cast(hydra.core.LiteralType, hydra.core.LiteralTypeString(None))))
            
            case hydra.core.LiteralTypeBoolean():
                return hydra.lib.maybes.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), adapt_integer_type(constraints, hydra.core.IntegerType.INT8))
            
            case hydra.core.LiteralTypeFloat(value=ft):
                return hydra.lib.maybes.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(x))), adapt_float_type(constraints, ft))
            
            case hydra.core.LiteralTypeInteger(value=it):
                return hydra.lib.maybes.map((lambda x: cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(x))), adapt_integer_type(constraints, it))
            
            case _:
                return cast(Maybe[hydra.core.LiteralType], Nothing())
    return hydra.lib.logic.if_else(literal_type_supported(constraints, lt), cast(Maybe[hydra.core.LiteralType], Nothing()), for_unsupported(lt))

def adapt_literal_types_map(constraints: hydra.coders.LanguageConstraints) -> FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType]:
    r"""Derive a map of adapted literal types for the given language constraints."""
    
    def try_type(lt: hydra.core.LiteralType) -> Maybe[Tuple[hydra.core.LiteralType, hydra.core.LiteralType]]:
        return hydra.lib.maybes.maybe(cast(Maybe[Tuple[hydra.core.LiteralType, hydra.core.LiteralType]], Nothing()), (lambda lt2: cast(Maybe[Tuple[hydra.core.LiteralType, hydra.core.LiteralType]], Just((lt, lt2)))), adapt_literal_type(constraints, lt))
    return cast(FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], hydra.lib.maps.from_list(hydra.lib.maybes.cat(hydra.lib.lists.map(try_type, hydra.variants.literal_types))))

def adapt_type_scheme[T0](constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], ts0: hydra.core.TypeScheme) -> hydra.compute.Flow[T0, hydra.core.TypeScheme]:
    vars0 = ts0.variables
    t0 = ts0.type
    return hydra.lib.flows.bind(adapt_type(constraints, litmap, t0), (lambda t1: hydra.lib.flows.pure(hydra.core.TypeScheme(vars0, t1))))

def adapt_primitive[T0](constraints: hydra.coders.LanguageConstraints, litmap: FrozenDict[hydra.core.LiteralType, hydra.core.LiteralType], prim0: hydra.graph.Primitive) -> hydra.compute.Flow[T0, hydra.graph.Primitive]:
    ts0 = prim0.type
    return hydra.lib.flows.bind(adapt_type_scheme(constraints, litmap, ts0), (lambda ts1: hydra.lib.flows.pure(hydra.graph.Primitive(prim0.name, ts1, (lambda v1: prim0.implementation(v1))))))

def adapt_literal(lt: hydra.core.LiteralType, l: hydra.core.Literal) -> hydra.core.Literal:
    r"""Convert a literal to a different type."""
    
    match l:
        case hydra.core.LiteralBinary(value=b):
            match lt:
                case hydra.core.LiteralTypeString():
                    return cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.binary_to_string(b)))
                
                case _:
                    raise TypeError("Unsupported LiteralType")
        
        case hydra.core.LiteralBoolean(value=b2):
            match lt:
                case hydra.core.LiteralTypeInteger(value=it):
                    return cast(hydra.core.Literal, hydra.core.LiteralInteger(hydra.literals.bigint_to_integer_value(it, hydra.lib.logic.if_else(b2, 1, 0))))
                
                case _:
                    raise TypeError("Unsupported LiteralType")
        
        case hydra.core.LiteralFloat(value=f):
            match lt:
                case hydra.core.LiteralTypeFloat(value=ft):
                    return cast(hydra.core.Literal, hydra.core.LiteralFloat(hydra.literals.bigfloat_to_float_value(ft, hydra.literals.float_value_to_bigfloat(f))))
                
                case _:
                    raise TypeError("Unsupported LiteralType")
        
        case hydra.core.LiteralInteger(value=i):
            match lt:
                case hydra.core.LiteralTypeInteger(value=it):
                    return cast(hydra.core.Literal, hydra.core.LiteralInteger(hydra.literals.bigint_to_integer_value(it, hydra.literals.integer_value_to_bigint(i))))
                
                case _:
                    raise TypeError("Unsupported LiteralType")
        
        case _:
            raise TypeError("Unsupported Literal")

def adapt_literal_value[T0](litmap: FrozenDict[T0, hydra.core.LiteralType], lt: T0, l: hydra.core.Literal) -> hydra.core.Literal:
    return hydra.lib.maybes.maybe(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.show.core.literal(l))), (lambda lt2: adapt_literal(lt2, l)), hydra.lib.maps.lookup(lt, litmap))

def term_alternatives(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.core.Term]]:
    r"""Find a list of alternatives for a given term, if any."""
    
    match term:
        case hydra.core.TermAnnotated(value=at):
            term2 = at.body
            return hydra.lib.flows.pure((term2,))
        
        case hydra.core.TermOptional(value=ot):
            return hydra.lib.flows.pure((cast(hydra.core.Term, hydra.core.TermList(hydra.lib.maybes.maybe(cast(frozenlist[hydra.core.Term], ()), (lambda term2: (term2,)), ot))),))
        
        case hydra.core.TermUnion(value=inj):
            tname = inj.type_name
            field = inj.field
            fname = field.name
            fterm = field.term
            def for_field_type(ft: hydra.core.FieldType) -> hydra.core.Field:
                ftname = ft.name
                return hydra.core.Field(fname, cast(hydra.core.Term, hydra.core.TermOptional(hydra.lib.logic.if_else(hydra.lib.equality.equal(ftname, fname), cast(Maybe[hydra.core.Term], Just(fterm)), cast(Maybe[hydra.core.Term], Nothing())))))
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
    
    def rewrite[T0](recurse: Callable[[T0], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], term02: T0) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]:
        def for_supported[T1](term: hydra.core.Term) -> hydra.compute.Flow[T1, Maybe[hydra.core.Term]]:
            match term:
                case hydra.core.TermLiteral(value=l):
                    lt = hydra.variants.literal_type(l)
                    return hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Just(hydra.lib.logic.if_else(literal_type_supported(constraints, lt), term, cast(hydra.core.Term, hydra.core.TermLiteral(adapt_literal_value(litmap, lt, l)))))))
                
                case _:
                    return hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Just(term)))
        def for_unsupported(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
            def for_non_null(alts: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
                return hydra.lib.flows.bind(try_term(hydra.lib.lists.head(alts)), (lambda mterm: hydra.lib.maybes.maybe(try_alts(hydra.lib.lists.tail(alts)), (lambda t: hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Just(t)))), mterm)))
            def try_alts(alts: frozenlist[hydra.core.Term]) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
                return hydra.lib.logic.if_else(hydra.lib.lists.null(alts), hydra.lib.flows.pure(cast(Maybe[hydra.core.Term], Nothing())), for_non_null(alts))
            return hydra.lib.flows.bind(term_alternatives(term), (lambda alts: try_alts(alts)))
        def try_term(term: hydra.core.Term) -> hydra.compute.Flow[hydra.graph.Graph, Maybe[hydra.core.Term]]:
            supported_variant = hydra.lib.sets.member(hydra.variants.term_variant(term), constraints.term_variants)
            return hydra.lib.logic.if_else(supported_variant, for_supported(term), for_unsupported(term))
        return hydra.lib.flows.bind(recurse(term02), (lambda term1: hydra.lib.flows.bind(try_term(term1), (lambda mterm: hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat(("no alternatives for term: ", hydra.show.core.term(term1)))), (lambda term2: hydra.lib.flows.pure(term2)), mterm)))))
    return hydra.rewriting.rewrite_term_m(cast(Callable[[
      Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]],
      hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, hydra.core.Term]], rewrite), term0)

def adapt_data_graph(constraints: hydra.coders.LanguageConstraints, do_expand: bool, graph0: hydra.graph.Graph) -> hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph]:
    r"""Adapt a graph and its schema to the given language constraints, prior to inference."""
    
    def expand[T0](graph: hydra.graph.Graph, gterm: hydra.core.Term) -> hydra.compute.Flow[T0, hydra.core.Term]:
        return hydra.lib.flows.bind(hydra.schemas.graph_to_type_context(graph), (lambda tx: hydra.lib.flows.bind(hydra.reduction.eta_expand_typed_term(tx, gterm), (lambda gterm1: hydra.lib.flows.pure(hydra.rewriting.lift_lambda_above_let(hydra.rewriting.unshadow_variables(gterm1)))))))
    litmap = adapt_literal_types_map(constraints)
    els0 = graph0.elements
    env0 = graph0.environment
    body0 = graph0.body
    prims0 = graph0.primitives
    schema0 = graph0.schema
    return hydra.lib.flows.bind(hydra.lib.maybes.maybe(hydra.lib.flows.pure(cast(Maybe[hydra.graph.Graph], Nothing())), (lambda sg: hydra.lib.flows.bind(hydra.schemas.graph_as_types(sg), (lambda tmap0: hydra.lib.flows.bind(adapt_graph_schema(constraints, litmap, tmap0), (lambda tmap1: (emap := hydra.schemas.types_to_elements(tmap1), hydra.lib.flows.pure(cast(Maybe[hydra.graph.Graph], Just(hydra.graph.Graph(emap, sg.environment, sg.types, sg.body, sg.primitives, sg.schema)))))[1]))))), schema0), (lambda schema1: (gterm0 := hydra.schemas.graph_as_term(graph0), hydra.lib.flows.bind(hydra.lib.logic.if_else(do_expand, expand(graph0, gterm0), hydra.lib.flows.pure(gterm0)), (lambda gterm1: hydra.lib.flows.bind(adapt_term(constraints, litmap, gterm1), (lambda gterm2: (els1 := hydra.schemas.term_as_graph(gterm2), hydra.lib.flows.bind(hydra.lib.flows.map_elems((lambda v1: adapt_primitive(constraints, litmap, v1)), prims0), (lambda prims1: hydra.lib.flows.pure(hydra.graph.Graph(els1, env0, cast(FrozenDict[hydra.core.Name, hydra.core.TypeScheme], hydra.lib.maps.empty()), cast(hydra.core.Term, hydra.core.TermUnit(None)), prims1, schema1)))))[1])))))[1]))

def data_graph_to_definitions(constraints: hydra.coders.LanguageConstraints, do_expand: bool, graph: hydra.graph.Graph, name_lists: frozenlist[frozenlist[hydra.core.Name]]) -> hydra.compute.Flow[hydra.graph.Graph, Tuple[hydra.graph.Graph, frozenlist[frozenlist[hydra.module.TermDefinition]]]]:
    r"""Given a data graph along with language constraints and a designated list of element names, adapt the graph to the language constraints, perform inference, then return a corresponding term definition for each element name."""
    
    return hydra.lib.flows.bind(hydra.lib.logic.if_else(do_expand, hydra.inference.infer_graph_types(graph), hydra.lib.flows.pure(graph)), (lambda graphi: hydra.lib.flows.bind(adapt_data_graph(constraints, do_expand, graphi), (lambda graph1: hydra.lib.flows.bind(hydra.inference.infer_graph_types(graph1), (lambda graph2: (to_def := (lambda el: (ts := hydra.lib.maybes.from_just(el.type), hydra.module.TermDefinition(el.name, el.term, hydra.schemas.type_scheme_to_f_type(ts)))[1]), hydra.lib.flows.pure((graph2, hydra.lib.lists.map((lambda names: hydra.lib.lists.map(to_def, hydra.lib.lists.map((lambda n: hydra.lib.maybes.from_just(hydra.lib.maps.lookup(n, graph2.elements))), names))), name_lists))))[1]))))))

def schema_graph_to_definitions(constraints: hydra.coders.LanguageConstraints, graph: hydra.graph.Graph, name_lists: frozenlist[frozenlist[hydra.core.Name]]) -> hydra.compute.Flow[hydra.graph.Graph, Tuple[FrozenDict[hydra.core.Name, hydra.core.Type], frozenlist[frozenlist[hydra.module.TypeDefinition]]]]:
    r"""Given a schema graph along with language constraints and a designated list of element names, adapt the graph to the language constraints, then return a corresponding type definition for each element name."""
    
    litmap = adapt_literal_types_map(constraints)
    return hydra.lib.flows.bind(hydra.schemas.graph_as_types(graph), (lambda tmap0: hydra.lib.flows.bind(adapt_graph_schema(constraints, litmap, tmap0), (lambda tmap1: (to_def := (lambda pair: hydra.module.TypeDefinition(pair[0], pair[1])), hydra.lib.flows.pure((tmap1, hydra.lib.lists.map((lambda names: hydra.lib.lists.map(to_def, hydra.lib.lists.map((lambda n: (n, hydra.lib.maybes.from_just(hydra.lib.maps.lookup(n, tmap1)))), names))), name_lists))))[1]))))
