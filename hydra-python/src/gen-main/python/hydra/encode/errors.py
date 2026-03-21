# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.errors."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.encode.core
import hydra.encode.error.checking
import hydra.encode.error.core
import hydra.errors

def decoding_error(x: hydra.errors.DecodingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.errors.DecodingError"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def other_error(x: hydra.errors.OtherError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.errors.OtherError"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def unification_error(x: hydra.errors.UnificationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), hydra.encode.core.type(x.left_type)), hydra.core.Field(hydra.core.Name("rightType"), hydra.encode.core.type(x.right_type)), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.message)))))))))

def error(v1: hydra.errors.Error) -> hydra.core.Term:
    match v1:
        case hydra.errors.ErrorChecking(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("checking"), hydra.encode.error.checking.checking_error(y)))))

        case hydra.errors.ErrorDecoding(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("decoding"), decoding_error(y2)))))

        case hydra.errors.ErrorDuplicateBinding(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("duplicateBinding"), hydra.encode.error.core.duplicate_binding_error(y3)))))

        case hydra.errors.ErrorDuplicateField(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("duplicateField"), hydra.encode.error.core.duplicate_field_error(y4)))))

        case hydra.errors.ErrorOther(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("other"), other_error(y5)))))

        case hydra.errors.ErrorUndefinedField(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("undefinedField"), hydra.encode.error.core.undefined_field_error(y6)))))

        case hydra.errors.ErrorUndefinedTerm(value=y7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("undefinedTerm"), hydra.encode.error.core.undefined_term_error(y7)))))

        case hydra.errors.ErrorUndefinedType(value=y8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("undefinedType"), hydra.encode.error.core.undefined_type_error(y8)))))

        case hydra.errors.ErrorUnexpectedTermVariant(value=y9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("unexpectedTermVariant"), hydra.encode.error.core.unexpected_term_variant_error(y9)))))

        case hydra.errors.ErrorUnexpectedTypeVariant(value=y10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("unexpectedTypeVariant"), hydra.encode.error.core.unexpected_type_variant_error(y10)))))

        case hydra.errors.ErrorUnification(value=y11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("unification"), unification_error(y11)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")
