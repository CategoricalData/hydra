"""A DSL for decoding Hydra terms."""

from typing import Generic, TypeVar

import hydra.dsl.flows as flows
from hydra.compute import Flow
from hydra.core import (
    Literal,
    LiteralBoolean,
    LiteralString,
    Term,
    TermLiteral,
    Type,
)

S = TypeVar("S")
X = TypeVar("X")


class Expect(Generic[S]):
    """Functions for decoding of native Python values from Hydra terms."""

    @staticmethod
    def literal(term: Term) -> Flow[S, Literal]:
        """Decode a literal value."""
        match term:
            case TermLiteral(value):
                return flows.pure(value)
            case _:
                return flows.fail(f"Expected a literal value, but got {term}")

    @staticmethod
    def string(term: Term) -> Flow[S, str]:
        """Decode a string value."""

        def handle_literal(literal: Literal) -> Flow[S, str]:
            match literal:
                case LiteralString(value):
                    return flows.pure(value)
                case _:
                    return flows.fail(f"Expected a string value, but got {literal}")

        return flows.bind(Expect[S].literal(term), handle_literal)

    @staticmethod
    def boolean(term: Term) -> Flow[S, bool]:
        """Decode a boolean value."""

        def handle_literal(literal: Literal) -> Flow[S, bool]:
            match literal:
                case LiteralBoolean(value):
                    return flows.pure(value)
                case _:
                    return flows.fail(f"Expected a boolean value, but got {literal}")

        return flows.bind(Expect[S].literal(term), handle_literal)

    @staticmethod
    def term(term: Term) -> Flow[S, Term]:
        """Decode a term."""
        return flows.pure(term)

    @staticmethod
    def type(term: Term) -> Flow[S, Type]:
        """Decode a type."""
        return flows.fail("Core decoding not yet implemented")
