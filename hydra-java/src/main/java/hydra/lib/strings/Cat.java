package hydra.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;

/**
 * Concatenates a list of strings into a single string.
 */
public class Cat extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.cat"
     */
    public Name name() {
        return new Name("hydra.lib.strings.cat");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that concatenates a list of strings
     */
    @Override
    public TypeScheme type() {
        return scheme(function(list(string()), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(strings -> Terms.string(apply(strings)), hydra.extract.core.Core.listOf(cx, t -> hydra.extract.core.Core.string(cx, graph, t), graph, args.get(0)));
    }

    /**
     * Concatenates a list of strings into a single string.
     * @param args the list of strings to concatenate
     * @return the concatenated string
     */
    public static String apply(List<String> args) {
        return String.join("", args);
    }
}
