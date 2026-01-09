# Note: this is an automatically generated file. Do not edit.

r"""Entry point for Hydra's adapter (type/term rewriting) framework."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import FrozenDict, frozenlist
from typing import TypeVar, cast
import hydra.adapt.terms
import hydra.adapt.utils
import hydra.annotations
import hydra.coders
import hydra.compute
import hydra.core
import hydra.decode.core
import hydra.graph
import hydra.lexical
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.monads
import hydra.rewriting
import hydra.schemas
import hydra.show.core
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def language_adapter(lang: hydra.coders.Language, typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.compute.Adapter[T0, T1, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    def get_pair(typ2: hydra.core.Type) -> hydra.compute.Flow[hydra.coders.AdapterContext, tuple[hydra.compute.Adapter[hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term], hydra.coders.AdapterContext]]:
        return hydra.lib.flows.bind(hydra.adapt.terms.term_adapter(typ2), (lambda ad: hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: hydra.lib.flows.pure((ad, cx))))))
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda g: (cx0 := (lambda : hydra.coders.AdapterContext(g, lang, hydra.lib.maps.empty())), hydra.lib.flows.bind(hydra.monads.with_state(cx0(), get_pair(typ)), (lambda result: (adapter := (lambda : hydra.lib.pairs.first(result)), cx := (lambda : hydra.lib.pairs.second(result)), encode := (lambda term: hydra.monads.with_state(cx(), adapter().coder.encode(term))), decode := (lambda term: hydra.monads.with_state(cx(), adapter().coder.decode(term))), hydra.lib.flows.pure(hydra.compute.Adapter(adapter().is_lossy, adapter().source, adapter().target, hydra.compute.Coder((lambda x1: encode(x1)), (lambda x1: decode(x1))))))[4])))[1]))

def adapt_type_to_language(lang: hydra.coders.Language, typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.core.Type]:
    return hydra.lib.flows.bind(language_adapter(lang, typ), (lambda adapter: hydra.lib.flows.pure(adapter.target)))

def adapt_type_to_language_and_encode(lang: hydra.coders.Language, enc: Callable[[hydra.core.Type], hydra.compute.Flow[hydra.graph.Graph, T0]], typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, T0]:
    def dflt() -> hydra.compute.Flow[hydra.graph.Graph, T0]:
        return hydra.lib.flows.bind(adapt_type_to_language(lang, typ), (lambda adapted_type: enc(adapted_type)))
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeVariable():
            return enc(typ)
        
        case _:
            return dflt()

def adapted_module_definitions(lang: hydra.coders.Language, mod: hydra.module.Module) -> hydra.compute.Flow[hydra.graph.Graph, frozenlist[hydra.module.Definition]]:
    return hydra.lib.flows.bind(hydra.monads.get_state(), (lambda cx: (els := mod.elements, adapters_for := (lambda types: hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: language_adapter(lang, v1)), types), (lambda adapters: hydra.lib.flows.pure(hydra.lib.maps.from_list(hydra.lib.lists.zip(types, adapters)))))), classify := (lambda adapters, pair: (el := (lambda : hydra.lib.pairs.first(pair)), tt := (lambda : hydra.lib.pairs.second(pair)), term := (lambda : tt().body), typ := (lambda : tt().type), name := (lambda : el().name), hydra.lib.logic.if_else(hydra.annotations.is_native_type(el()), (lambda : hydra.lib.flows.bind(hydra.lib.flows.bind(hydra.monads.with_trace("adapt module definitions", hydra.monads.either_to_flow((lambda v1: v1.value), hydra.decode.core.type(cx, term()))), (lambda core_typ: adapt_type_to_language(lang, core_typ))), (lambda adapted_typ: hydra.lib.flows.pure(cast(hydra.module.Definition, hydra.module.DefinitionType(hydra.module.TypeDefinition(name(), adapted_typ))))))), (lambda : hydra.lib.maybes.maybe(hydra.lib.flows.fail(hydra.lib.strings.cat2("no adapter for element ", name().value)), (lambda adapter: hydra.lib.flows.bind(adapter.coder.encode(term()), (lambda adapted: hydra.lib.flows.pure(cast(hydra.module.Definition, hydra.module.DefinitionTerm(hydra.module.TermDefinition(name(), adapted, hydra.schemas.type_to_type_scheme(adapter.target)))))))), hydra.lib.maps.lookup(typ(), adapters)))))[5]), hydra.lib.flows.bind(hydra.lexical.with_schema_context(hydra.lib.flows.map_list((lambda x1: hydra.schemas.element_as_type_application_term(x1)), els)), (lambda tterms: (types := (lambda : hydra.lib.sets.to_list(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda arg_: hydra.rewriting.deannotate_type(arg_.type)), tterms)))), hydra.lib.flows.bind(adapters_for(types()), (lambda adapters: hydra.lib.flows.map_list((lambda v1: classify(adapters, v1)), hydra.lib.lists.zip(els, tterms)))))[1])))[3]))

def construct_coder(lang: hydra.coders.Language, encode_term: Callable[[hydra.core.Term], hydra.compute.Flow[T0, T1]], typ: hydra.core.Type) -> hydra.compute.Flow[hydra.graph.Graph, hydra.compute.Coder[T0, T2, hydra.core.Term, T1]]:
    return hydra.monads.with_trace(hydra.lib.strings.cat2("coder for ", hydra.show.core.type(typ)), hydra.lib.flows.bind(language_adapter(lang, typ), (lambda adapter: hydra.lib.flows.pure(hydra.adapt.utils.compose_coders(adapter.coder, hydra.adapt.utils.unidirectional_coder(encode_term))))))

def transform_module(lang: hydra.coders.Language, encode_term: Callable[[hydra.core.Term], hydra.compute.Flow[T0, T1]], create_module: Callable[[
  hydra.module.Module,
  FrozenDict[hydra.core.Type, hydra.compute.Coder[T0, T2, hydra.core.Term, T1]],
  frozenlist[tuple[hydra.core.Binding, hydra.core.TypeApplicationTerm]]], hydra.compute.Flow[hydra.graph.Graph, T3]], mod: hydra.module.Module) -> hydra.compute.Flow[hydra.graph.Graph, T3]:
    els = mod.elements
    def transform() -> hydra.compute.Flow[hydra.graph.Graph, T3]:
        return hydra.lib.flows.bind(hydra.lexical.with_schema_context(hydra.lib.flows.map_list((lambda x1: hydra.schemas.element_as_type_application_term(x1)), els)), (lambda tterms: (types := (lambda : hydra.lib.lists.nub(hydra.lib.lists.map((lambda v1: v1.type), tterms))), hydra.lib.flows.bind(hydra.lib.flows.map_list((lambda v1: construct_coder(lang, encode_term, v1)), types()), (lambda cdrs: (coders := (lambda : hydra.lib.maps.from_list(hydra.lib.lists.zip(types(), cdrs))), create_module(mod, coders(), hydra.lib.lists.zip(els, tterms)))[1])))[1]))
    return hydra.monads.with_trace(hydra.lib.strings.cat2("transform module ", mod.namespace.value), transform())
