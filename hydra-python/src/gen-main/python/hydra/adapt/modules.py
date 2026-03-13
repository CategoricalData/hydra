# Note: this is an automatically generated file. Do not edit.

r"""Entry point for Hydra's adapter (type/term rewriting) framework."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt.terms
import hydra.adapt.utils
import hydra.annotations
import hydra.coders
import hydra.compute
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.error
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.module
import hydra.rewriting
import hydra.schemas
import hydra.show.error

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def language_adapter(lang: hydra.coders.Language, _cx: T0, g: hydra.graph.Graph, typ: hydra.core.Type) -> Either[str, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]:
    r"""Given a target language and a source type, produce an adapter, which rewrites the type and its terms according to the language's constraints."""
    
    @lru_cache(1)
    def cx0() -> hydra.coders.AdapterContext:
        return hydra.coders.AdapterContext(g, lang, hydra.lib.maps.empty())
    return hydra.adapt.terms.term_adapter(cx0(), typ)

def adapt_type_to_language(lang: hydra.coders.Language, cx: T0, g: hydra.graph.Graph, typ: hydra.core.Type) -> Either[str, hydra.core.Type]:
    r"""Given a target language and a source type, find the target type to which the latter will be adapted."""
    
    return hydra.lib.eithers.map((lambda v1: v1.target), language_adapter(lang, cx, g, typ))

def adapt_type_to_language_and_encode(lang: hydra.coders.Language, enc: Callable[[hydra.core.Type], Either[str, T0]], cx: T1, g: hydra.graph.Graph, typ: hydra.core.Type) -> Either[str, T0]:
    r"""Given a target language, an encoding function, and a type, adapt and encode the type."""
    
    @lru_cache(1)
    def dflt() -> Either[str, T0]:
        return hydra.lib.eithers.bind(adapt_type_to_language(lang, cx, g, typ), (lambda adapted_type: enc(adapted_type)))
    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeVariable():
            return enc(typ)
        
        case _:
            return dflt()

def adapted_module_definitions(lang: hydra.coders.Language, cx: hydra.context.Context, graph: hydra.graph.Graph, mod: hydra.module.Module) -> Either[str, frozenlist[hydra.module.Definition]]:
    r"""Map a Hydra module to a list of type and/or term definitions which have been adapted to the target language."""
    
    els = mod.elements
    def adapters_for(types: frozenlist[hydra.core.Type]) -> Either[str, FrozenDict[hydra.core.Type, hydra.compute.Adapter[hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term]]]:
        return hydra.lib.eithers.map((lambda adapters: hydra.lib.maps.from_list(hydra.lib.lists.zip(types, adapters))), hydra.lib.eithers.map_list((lambda v1: language_adapter(lang, cx, graph, v1)), types))
    def classify(adapters: FrozenDict[hydra.core.Type, hydra.compute.Adapter[T0, hydra.core.Type, hydra.core.Term, hydra.core.Term]], pair: tuple[hydra.core.Binding, hydra.core.TypeApplicationTerm]) -> Either[str, hydra.module.Definition]:
        @lru_cache(1)
        def el() -> hydra.core.Binding:
            return hydra.lib.pairs.first(pair)
        @lru_cache(1)
        def tt() -> hydra.core.TypeApplicationTerm:
            return hydra.lib.pairs.second(pair)
        term = tt().body
        typ = tt().type
        name = el().name
        return hydra.lib.logic.if_else(hydra.annotations.is_native_type(el()), (lambda : hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda e: e.value), (lambda x: x), hydra.decode.core.type(graph, term)), (lambda core_typ: hydra.lib.eithers.bind(adapt_type_to_language(lang, cx, graph, core_typ), (lambda adapted_typ: Right(cast(hydra.module.Definition, hydra.module.DefinitionType(hydra.module.TypeDefinition(name, adapted_typ))))))))), (lambda : hydra.lib.maybes.maybe((lambda : Left(hydra.lib.strings.cat2("no adapter for element ", name.value))), (lambda adapter: hydra.lib.eithers.bind(hydra.lib.eithers.bimap((lambda ic: hydra.show.error.error(ic.object)), (lambda x: x), adapter.coder.encode(cx, term)), (lambda adapted: Right(cast(hydra.module.Definition, hydra.module.DefinitionTerm(hydra.module.TermDefinition(name, adapted, hydra.schemas.type_to_type_scheme(adapter.target)))))))), hydra.lib.maps.lookup(typ, adapters))))
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda _el: hydra.lib.eithers.bimap((lambda ic: hydra.show.error.error(ic.object)), (lambda x: x), hydra.schemas.element_as_type_application_term(cx, _el))), els), (lambda tterms: (types := hydra.lib.sets.to_list(hydra.lib.sets.from_list(hydra.lib.lists.map((lambda arg_: hydra.rewriting.deannotate_type(arg_.type)), tterms))), hydra.lib.eithers.bind(adapters_for(types), (lambda adapters: hydra.lib.eithers.map_list((lambda v1: classify(adapters, v1)), hydra.lib.lists.zip(els, tterms)))))[1]))

def construct_coder(lang: hydra.coders.Language, encode_term: Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T0]], cx: T1, g: hydra.graph.Graph, typ: hydra.core.Type) -> Either[str, hydra.compute.Coder[hydra.core.Term, T0]]:
    r"""Given a target language, a unidirectional last-mile encoding, and a source type, construct a unidirectional adapting coder for terms of that type."""
    
    return hydra.lib.eithers.map((lambda adapter: hydra.adapt.utils.compose_coders(adapter.coder, hydra.adapt.utils.unidirectional_coder(encode_term))), language_adapter(lang, cx, g, typ))

def transform_module(lang: hydra.coders.Language, encode_term: Callable[[hydra.context.Context, hydra.core.Term], Either[hydra.context.InContext[hydra.error.Error], T0]], create_module: Callable[[
  hydra.module.Module,
  FrozenDict[hydra.core.Type, hydra.compute.Coder[hydra.core.Term, T0]],
  frozenlist[tuple[hydra.core.Binding, hydra.core.TypeApplicationTerm]]], Either[str, T1]], cx: hydra.context.Context, g: hydra.graph.Graph, mod: hydra.module.Module) -> Either[str, T1]:
    r"""Given a target language, a unidirectional last mile encoding, and an intermediate helper function, transform a given module into a target representation."""
    
    els = mod.elements
    return hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda _el: hydra.lib.eithers.bimap((lambda ic: hydra.show.error.error(ic.object)), (lambda x: x), hydra.schemas.element_as_type_application_term(cx, _el))), els), (lambda tterms: (types := hydra.lib.lists.nub(hydra.lib.lists.map((lambda v1: v1.type), tterms)), hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: construct_coder(lang, encode_term, cx, g, v1)), types), (lambda cdrs: (coders := hydra.lib.maps.from_list(hydra.lib.lists.zip(types, cdrs)), create_module(mod, coders, hydra.lib.lists.zip(els, tterms)))[1])))[1]))
