"""Tools for Hydra."""

from abc import ABC
from collections.abc import Callable

import hydra.dsl.terms as terms
from hydra.compute import Flow
from hydra.core import Name, Term, TypeScheme
from hydra.dsl.python import frozenlist
from hydra.graph import Graph, Primitive


class PrimitiveFunction(ABC):
    """An abstract base class for all hydra primitive functions."""

    def name(self) -> Name:
        """Get the name of the primitive function."""
        raise NotImplementedError("Not implemented")

    def type(self) -> TypeScheme:
        """Get the type scheme of the primitive function."""
        raise NotImplementedError("Not implemented")

    def implementation(self) -> Callable[[frozenlist[Term]], Flow[Graph, Term]]:
        """Get the implementation of the primitive function."""
        raise NotImplementedError("Not implemented")

    def term(self) -> Term:
        """Get the term of the primitive function."""
        return terms.primitive(self.name())

    def to_native(self) -> Primitive:
        """Get the native primitive function."""
        return Primitive(self.name(), self.type(), self.implementation())
