# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.error.checking."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.decode.core
import hydra.decode.typing
import hydra.decode.variants
import hydra.error.checking
import hydra.errors
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings

def incorrect_unification_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_incorrect_unification_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("substitution", (lambda x1, x2: hydra.decode.typing.type_subst(x1, x2)), field_map(), cx), (lambda field_substitution: Right(hydra.error.checking.IncorrectUnificationError(field_substitution))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_incorrect_unification_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def not_a_forall_type_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_not_a_forall_type_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeArguments", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: hydra.decode.core.type(x1, x2)), v12, v2)), field_map(), cx), (lambda field_type_arguments: Right(hydra.error.checking.NotAForallTypeError(field_type, field_type_arguments))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_not_a_forall_type_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def not_a_function_type_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_not_a_function_type_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_type: Right(hydra.error.checking.NotAFunctionTypeError(field_type))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_not_a_function_type_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_arity_mismatch_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_type_arity_mismatch_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12):
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 value"))
                def _hoist_body_2(v12):
                    match v12:
                        case hydra.core.LiteralInteger(value=_match_value):
                            return _hoist_body_1(_match_value)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 literal"))
                def _hoist_body_3(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_2(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_4(v12):
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 value"))
                def _hoist_body_5(v12):
                    match v12:
                        case hydra.core.LiteralInteger(value=_match_value):
                            return _hoist_body_4(_match_value)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 literal"))
                def _hoist_body_6(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_5(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expectedArity", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_expected_arity: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("actualArity", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_actual_arity: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeArguments", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: hydra.decode.core.type(x1, x2)), v12, v2)), field_map(), cx), (lambda field_type_arguments: Right(hydra.error.checking.TypeArityMismatchError(field_type, field_expected_arity, field_actual_arity, field_type_arguments))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_type_arity_mismatch_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_mismatch_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_type_mismatch_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expectedType", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_expected_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("actualType", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_actual_type: Right(hydra.error.checking.TypeMismatchError(field_expected_type, field_actual_type))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_type_mismatch_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def unbound_type_variables_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_unbound_type_variables_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("variables", (lambda v12, v2: hydra.extract.helpers.decode_set((lambda x1, x2: hydra.decode.core.name(x1, x2)), v12, v2)), field_map(), cx), (lambda field_variables: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_type: Right(hydra.error.checking.UnboundTypeVariablesError(field_variables, field_type))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_unbound_type_variables_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def unequal_types_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_unequal_types_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("types", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: hydra.decode.core.type(x1, x2)), v12, v2)), field_map(), cx), (lambda field_types: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("description", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_description: Right(hydra.error.checking.UnequalTypesError(field_types, field_description))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_unequal_types_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def unsupported_term_variant_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_unsupported_term_variant_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("termVariant", (lambda x1, x2: hydra.decode.variants.term_variant(x1, x2)), field_map(), cx), (lambda field_term_variant: Right(hydra.error.checking.UnsupportedTermVariantError(field_term_variant))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_unsupported_term_variant_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def untyped_lambda_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_untyped_lambda_error_1(v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return Right(hydra.error.checking.UntypedLambdaError())

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_untyped_lambda_error_1(stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def untyped_let_binding_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_untyped_let_binding_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("binding", (lambda x1, x2: hydra.decode.core.binding(x1, x2)), field_map(), cx), (lambda field_binding: Right(hydra.error.checking.UntypedLetBindingError(field_binding))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_untyped_let_binding_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def checking_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_checking_checking_error_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.error.checking.CheckingError]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("incorrectUnification"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorIncorrectUnification(t))), incorrect_unification_error(cx, input)))), (hydra.core.Name("notAForallType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorNotAForallType(t))), not_a_forall_type_error(cx, input)))), (hydra.core.Name("notAFunctionType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorNotAFunctionType(t))), not_a_function_type_error(cx, input)))), (hydra.core.Name("typeArityMismatch"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeArityMismatch(t))), type_arity_mismatch_error(cx, input)))), (hydra.core.Name("typeMismatch"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorTypeMismatch(t))), type_mismatch_error(cx, input)))), (hydra.core.Name("unboundTypeVariables"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUnboundTypeVariables(t))), unbound_type_variables_error(cx, input)))), (hydra.core.Name("unequalTypes"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUnequalTypes(t))), unequal_types_error(cx, input)))), (hydra.core.Name("unsupportedTermVariant"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUnsupportedTermVariant(t))), unsupported_term_variant_error(cx, input)))), (hydra.core.Name("untypedLambda"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUntypedLambda(t))), untyped_lambda_error(cx, input)))), (hydra.core.Name("untypedLetBinding"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.checking.CheckingError, hydra.error.checking.CheckingErrorUntypedLetBinding(t))), untyped_let_binding_error(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_checking_checking_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
