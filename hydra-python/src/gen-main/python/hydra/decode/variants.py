# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.variants."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left
from typing import cast
import hydra.core
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util
import hydra.variants

def elimination_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.EliminationVariant]:
    def _hoist_hydra_decode_variants_elimination_variant_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.EliminationVariant]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Name:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Field:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Name:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Term:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.variants.EliminationVariant]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("record"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.EliminationVariant.RECORD), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("union"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.EliminationVariant.UNION), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("wrap"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.EliminationVariant.WRAP), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.variants.EliminationVariant"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_variants_elimination_variant_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def function_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.FunctionVariant]:
    def _hoist_hydra_decode_variants_function_variant_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.FunctionVariant]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Name:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Field:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Name:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Term:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.variants.FunctionVariant]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("elimination"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.FunctionVariant.ELIMINATION), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("lambda"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.FunctionVariant.LAMBDA), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("primitive"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.FunctionVariant.PRIMITIVE), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.variants.FunctionVariant"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_variants_function_variant_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def literal_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.LiteralVariant]:
    def _hoist_hydra_decode_variants_literal_variant_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.LiteralVariant]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Name:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Field:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Name:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Term:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.variants.LiteralVariant]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("binary"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.LiteralVariant.BINARY), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("boolean"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.LiteralVariant.BOOLEAN), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("float"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.LiteralVariant.FLOAT), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("integer"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.LiteralVariant.INTEGER), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("string"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.LiteralVariant.STRING), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.variants.LiteralVariant"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_variants_literal_variant_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.TermVariant]:
    def _hoist_hydra_decode_variants_term_variant_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.TermVariant]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Name:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Field:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Name:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Term:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.variants.TermVariant]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("annotated"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.ANNOTATED), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("application"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.APPLICATION), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("either"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.EITHER), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("function"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.FUNCTION), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("let"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.LET), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("list"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.LIST), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("literal"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.LITERAL), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("map"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.MAP), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("maybe"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.MAYBE), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("pair"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.PAIR), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("record"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.RECORD), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("set"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.SET), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("typeApplication"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.TYPE_APPLICATION), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("typeLambda"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.TYPE_LAMBDA), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("union"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.UNION), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("unit"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.UNIT), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("variable"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.VARIABLE), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("wrap"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TermVariant.WRAP), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.variants.TermVariant"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_variants_term_variant_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_variant(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.TypeVariant]:
    def _hoist_hydra_decode_variants_type_variant_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.variants.TypeVariant]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                @lru_cache(1)
                def tname() -> hydra.core.Name:
                    return inj.type_name
                @lru_cache(1)
                def field() -> hydra.core.Field:
                    return inj.field
                @lru_cache(1)
                def fname() -> hydra.core.Name:
                    return field().name
                @lru_cache(1)
                def fterm() -> hydra.core.Term:
                    return field().term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.variants.TypeVariant]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("annotated"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.ANNOTATED), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("application"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.APPLICATION), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("either"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.EITHER), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("forall"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.FORALL), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("function"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.FUNCTION), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("list"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.LIST), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("literal"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.LITERAL), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("map"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.MAP), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("maybe"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.MAYBE), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("pair"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.PAIR), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("record"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.RECORD), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("set"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.SET), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("union"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.UNION), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("unit"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.UNIT), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("variable"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.VARIABLE), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("wrap"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.variants.TypeVariant.WRAP), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.variants.TypeVariant"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_variants_type_variant_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
