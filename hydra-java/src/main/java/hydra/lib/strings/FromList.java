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
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;

/**
 * Converts a list of character code points to a string.
 */
public class FromList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.fromList"
     */
    public Name name() {
        return new Name("hydra.lib.strings.fromList");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that converts a list of integers to a string
     */
    @Override
    public TypeScheme type() {
        return scheme(function(list(int32()), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> {
            Either<InContext<Error_>, List<Integer>> list = hydra.extract.core.Core.listOf(cx, t -> hydra.extract.core.Core.int32(cx, graph, t), graph, args.get(0));
            return hydra.lib.eithers.Map.apply(l -> Terms.string(FromList.apply(l)), list);
        };
    }

    /**
     * Converts a list of character code points to a string.
     * @param list the list of character code points
     * @return the resulting string
     */
    public static String apply(List<Integer> list) {
        StringBuilder sb = new StringBuilder();
        for (Integer i : list) {
            sb.appendCodePoint(i);
        }
        return sb.toString();
    }
}
