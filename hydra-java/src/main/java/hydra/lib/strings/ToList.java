package hydra.lib.strings;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.ConsList;

import java.util.ArrayList;
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<String, Term>) s -> {
            List<Integer> list = apply(s);
            List<Term> terms = new ArrayList<>(list.size());
            for (Integer i : list) {
                terms.add(Terms.int32(i));
            }
            return Terms.list(terms);
        }, hydra.extract.core.Core.string(cx, graph, args.get(0)));
    }

    /**
     * Converts a string to a list of character code points.
     * @param s the string to convert
     * @return the list of character code points
     */
    public static ConsList<Integer> apply(String s) {
        ArrayList<Integer> list = new ArrayList<>(s.codePointCount(0, s.length()));
        s.codePoints().forEach(list::add);
        return ConsList.fromList(list);
    }
}
