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
import hydra.errors.Error_;
import hydra.util.ConsList;
import hydra.util.Either;

/**
 * Converts a string to a list of character code points.
 */
public class ToList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.strings.toList"
     */
    public Name name() {
        return new Name("hydra.lib.strings.toList");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that converts a string to a list of integers
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), list(int32())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<String, Term>) s -> {
            ConsList<Term> reversed = ConsList.empty();
            for (Integer i : apply(s)) {
                reversed = ConsList.cons(Terms.int32(i), reversed);
            }
            return Terms.list(reversed.reverse());
        }, hydra.extract.Core.string(graph, args.get(0)));
    }

    /**
     * Converts a string to a list of character code points.
     * @param s the string to convert
     * @return the list of character code points
     */
    public static List<Integer> apply(String s) {
        // Walk the string in reverse so we can prepend onto a ConsList in O(1) per char.
        ConsList<Integer> result = ConsList.empty();
        int i = s.length();
        while (i > 0) {
            int cp = s.codePointBefore(i);
            result = ConsList.cons(cp, result);
            i -= Character.charCount(cp);
        }
        return result;
    }
}
