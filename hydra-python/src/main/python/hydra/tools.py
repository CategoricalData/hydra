# package hydra.tools;

# import hydra.compute.Flow;
# import hydra.core.Name;
# import hydra.core.Term;
# import hydra.core.TypeScheme;
# import hydra.dsl.Terms;
# import hydra.graph.Graph;
# import hydra.graph.Primitive;

# import java.util.List;
# import java.util.function.Function;


# /**
#  * Any of Hydra's primitive functions, implemented in Java
#  */
# public abstract class PrimitiveFunction {
#     /**
#      * The unique name of the primitive function
#      */
#     public abstract Name name();

#     /**
#      * The datatype of the primitive function
#      */
#     public abstract TypeScheme type();

#     /**
#      * A dynamic/interpreted implementation of the function
#      */
#     protected abstract Function<List<Term>, Flow<Graph, Term>> implementation();

#     /**
#      * The primitive function as a term
#      */
#     public Term term() {
#         return Terms.primitive(name());
#     }

#     /**
#      * The primitive function as a native Hydra Primitive object
#      */
#     public Primitive toNative() {
#         return new Primitive(name(), type(), implementation());
#     }
# }

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
