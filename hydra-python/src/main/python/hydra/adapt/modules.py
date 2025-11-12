# Note: this is an automatically generated file. Do not edit.

r"""Entry point for Hydra's adapter (type/term rewriting) framework."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, frozenlist
from typing import Tuple, cast
import hydra.adapt.terms
import hydra.adapt.utils
import hydra.annotations
import hydra.coders
import hydra.compute
import hydra.core
import hydra.decode.core
import hydra.describe.core
import hydra.graph
import hydra.lexical
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.rewriting
import hydra.schemas

def language_adapter[T0, T1](lang: hydra.coders.Language, typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.compute.Adapter[T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    def get_pair(typ2: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, Tuple[hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term], hydra.coders.AdapterContext]]:
        return hydra.lib.flows.bind(hydra.adapt.terms.term_adapter(typ2), (lambda ad: hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.coders.AdapterContext, hydra.coders.AdapterContext], hydra.monads.get_state), (lambda cx: hydra.lib.flows.pure((ad, cx))))))
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.graph.Graph, hydra.graph.Graph], hydra.monads.get_state), (lambda g: (cx0 := hydra.coders.AdapterContext(g, lang, cast(FrozenDict[hydra.core.Name, hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]], hydra.lib.maps.empty())), hydra.lib.flows.bind(hydra.monads.with_state(cx0, get_pair(typ)), (lambda result: (adapter := result[0], cx := result[1], encode := (lambda term: hydra.monads.with_state(cx, adapter.coder.encode(term))), decode := (lambda term: hydra.monads.with_state(cx, adapter.coder.decode(term))), hydra.lib.flows.pure(cast(hydra.compute.Adapter[T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term], hydra.compute.Adapter(adapter.is_lossy, adapter.source, adapter.target, cast(hydra.compute.Coder[T0, T1, hydra.core.Term, hydra.core.Term], hydra.compute.Coder(cast(Callable[[hydra.core.Term], hydra.compute.Flow[T0, hydra.core.Term]], encode), cast(Callable[[hydra.core.Term], hydra.compute.Flow[T1, hydra.core.Term]], decode)))))))[4])))[1]))

def adapt_type_to_language[T1, T0](lang: hydra.coders.Language, typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    return hydra.lib.flows.bind(language_adapter(lang, typ), (lambda adapter: hydra.lib.flows.pure(adapter.target)))

def adapt_type_to_language_and_encode[T0](lang: hydra.coders.Language, enc: Callable[[hydra.core.Type], hydra.compute.Flow[hydra.graph.Graph, T0]], typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, T0]:
    dflt = hydra.lib.flows.bind(adapt_type_to_language(lang, typ), (lambda adapted_type: enc(adapted_type)))
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeVariable():
            return enc(typ)
        
        case _:
            return dflt

def adapted_module_definitions[T0](lang: hydra.coders.Language, mod: hydra.module.Module) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.module.Definition]]:
    els = mod.elements
    def adapters_for[T1, T2](types: frozenlist[hydra.core.Type]) -> hydra.compute.Flow[hydra.graph.Graph, FrozenDict[hydra.core.Type, hydra.compute.Adapter[T1, T2, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]:
        return hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: language_adapter(lang, v1)), types), (lambda adapters: hydra.lib.flows.pure(cast(FrozenDict[hydra.core.Type, hydra.compute.Adapter[T1, T2, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]], hydra.lib.maps.from_list(hydra.lib.lists.zip(types, adapters))))))
    def classify[T1, T2](adapters: FrozenDict[hydra.core.Type, hydra.compute.Adapter[hydra.graph.Graph, T1, T2, hydra.core.Type, hydra.core.Term, hydra.core.Term]], tuple2: Tuple[hydra.core.Binding, hydra.core.TypeApplicationTerm]) -> hydra.compute.Flow[hydra.graph.Graph, hydra.module.Definition]:
        el = tuple2[0]
        tt = tuple2[1]
        term = tt.body
        typ = tt.type
        name = el.name
        return hydra.lib.logic.if_else(hydra.annotations.is_native_type(el), hydra.lib.flows.bind(hydra.lib.flows.bind(hydra.monads.with_trace("adapt module definitions", hydra.decode.core.type(term)), (lambda core_typ: adapt_type_to_language(lang, core_typ))), (lambda adapted_typ: hydra.lib.flows.pure(cast(hydra.module.Definition, hydra.module.DefinitionType(hydra.module.TypeDefinition(name, adapted_typ)))))), hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("no adapter for element ", name.value)), (lambda adapter: hydra.lib.flows.bind(adapter.coder.encode(term), (lambda adapted: hydra.lib.flows.pure(cast(hydra.module.Definition, hydra.module.DefinitionTerm(hydra.module.TermDefinition(name, adapted, adapter.target))))))), hydra.lib.maps.lookup(typ, adapters)))
    return hydra.lib.flows.bind(hydra.lexical.with_schema_context(hydra.lib.flows.map_list(cast(Callable[[hydra.core.Binding], hydra.compute.Flow[hydra.graph.Graph, hydra.core.TypeApplicationTerm]], hydra.schemas.element_as_type_application_term), els)), (lambda tterms: (types := hydra.lib.sets.to_list(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda arg_: hydra.rewriting.deannotate_type(arg_.type)), tterms))), hydra.lib.flows.bind(adapters_for(types), (lambda adapters: hydra.lib.flows.map_list((lambda v1: classify(adapters, v1)), hydra.lib.lists.zip(els, tterms)))))[1]))

def construct_coder[T0, T1, T2](lang: hydra.coders.Language, encode_term: Callable[[hydra.core.Term], hydra.compute.Flow[T0, T1]], typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.compute.Coder[T0, T2, hydra.core.Term, T1]]:
    return hydra.monads.with_trace(hydra.lib.strings.cat2("coder for ", hydra.describe.core.type(typ)), hydra.lib.flows.bind(language_adapter(lang, typ), (lambda adapter: hydra.lib.flows.pure(hydra.adapt.utils.compose_coders(adapter.coder, hydra.adapt.utils.unidirectional_coder(encode_term))))))

def transform_module[T0, T1, T2, T3](lang: hydra.coders.Language, encode_term: Callable[[hydra.core.Term], hydra.compute.Flow[T0, T1]], create_module: Callable[[
  hydra.module.Module,
  FrozenDict[hydra.core.Type, hydra.compute.Coder[T0, T2, hydra.core.Term, T1]],
  frozenlist[Tuple[hydra.core.Binding, hydra.core.TypeApplicationTerm]]], hydra.compute.Flow[hydra.graph.Graph, T3]], mod: hydra.module.Module) -> hydra.compute.Flow[hydra.graph.Graph, T3]:
    els = mod.elements
    transform = hydra.lib.flows.bind(hydra.lexical.with_schema_context(hydra.lib.flows.map_list(cast(Callable[[hydra.core.Binding], hydra.compute.Flow[hydra.graph.Graph, hydra.core.TypeApplicationTerm]], hydra.schemas.element_as_type_application_term), els)), (lambda tterms: (types := hydra.lib.lists.nub(hydra.lib.lists.map((lambda v1: v1.type), tterms)), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: construct_coder(lang, encode_term, v1)), types), (lambda cdrs: (coders := cast(FrozenDict[hydra.core.Type, hydra.compute.Coder[T0, T2, hydra.core.Term, T1]], hydra.lib.maps.from_list(hydra.lib.lists.zip(types, cdrs))), create_module(mod, coders, hydra.lib.lists.zip(els, tterms)))[1])))[1]))
    return hydra.monads.with_trace(hydra.lib.strings.cat2("transform module ", mod.namespace.value), transform)
