package hydra.core.overlay.java.lib.strings;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.int32;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Converts a list of character code points to a string.
 */
public class FromList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.strings.fromList"
     */
    public Name name() {
        return hydra.core.lib.Strings.fromList().name;
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
            Either<Error_, java.util.List<Integer>> list = hydra.core.extract.Model.listOf(t -> hydra.core.extract.Model.int32(graph, t), graph, args.get(0));
            return hydra.core.overlay.java.lib.eithers.Map.apply(l -> Terms.string(FromList.apply(l)), list);
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
