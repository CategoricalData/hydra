package hydra.lib.sets;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.schemeOrd;
import static hydra.dsl.Types.set;
import hydra.typing.InferenceContext;
import hydra.errors.Error_;
import hydra.util.ConsList;
import hydra.util.Either;


/**
 * Converts a set to a list of elements.
 */
public class ToList extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.sets.toList"
     */
    public Name name() {
        return new Name("hydra.lib.sets.toList");
    }

    /**
     * Returns the type scheme of this function.
     * @return the type scheme for a function that converts a set to a list
     */
    @Override
    public TypeScheme type() {
        return schemeOrd("x", function(set("x"), list("x")));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that transforms terms to a flow of graph and term
     */
    @Override
    protected Function<List<Term>, Function<InferenceContext, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply(terms -> {
            // Term needs a custom comparator (compareTerms), so sort in an ArrayList scratch
            // buffer and convert the sorted result to a ConsList.
            ArrayList<Term> scratch = new ArrayList<>(terms);
            scratch.sort(hydra.lib.equality.Compare::compareTerms);
            return Terms.list(ConsList.fromList(scratch));
        }, hydra.extract.Core.set(graph, args.get(0)));
    }

    /**
     * Converts a set to a list of elements.
     * @param <X> the type of elements in the set and list
     * @param arg the set to convert
     * @return a list containing all elements from the set
     */
    public static <X> List<X> apply(Set<X> arg) {
        ConsList<X> reversed = ConsList.empty();
        for (X x : arg) {
            reversed = ConsList.cons(x, reversed);
        }
        return reversed.reverse();
    }
}
