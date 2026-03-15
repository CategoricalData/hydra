package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import hydra.util.PersistentSet;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.schemeOrd;
import static hydra.dsl.Types.set;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Returns the number of elements in a set.
 */
public class Size extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.size"
     */
    public Name name() {
        return new Name("hydra.lib.sets.size");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that returns the size of a set
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(set("x"), int32()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(arg -> Terms.int32(apply(arg)), hydra.extract.core.Core.set(cx, graph, args.get(0)));
    }

    /**
     * Returns the number of elements in a set.
     * @param <X> the type of elements in the set
     * @param arg the set to measure
     * @return the number of elements in the set
     */
    public static <X> Integer apply(PersistentSet<X> arg) {
        return arg.size();
    }
}
