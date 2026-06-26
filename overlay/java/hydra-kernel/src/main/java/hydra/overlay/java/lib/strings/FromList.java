package hydra.overlay.java.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Converts a list of character code points to a string.
 */
public class FromList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.fromList"
     */
    public Name name() {
        return hydra.lib.Strings.fromList().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> {
            Either<Error_, java.util.List<Integer>> list = hydra.extract.Core.listOf(t -> hydra.extract.Core.int32(graph, t), graph, args.get(0));
            return hydra.overlay.java.lib.eithers.Map.apply(l -> Terms.string(FromList.apply(l)), list);
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
