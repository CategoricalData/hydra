package hydra.overlay.java.lib.optionals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Filters and extracts Just values.
 */
public class Cat extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.optionals.cat"
     */
    public Name name() {
        return hydra.lib.Optionals.cat().name;
    }

    /**
     * Returns the type scheme of this primitive function.
     * @return the type scheme for extracting present values from a list of optionals
     */
    @Override
    public TypeScheme type() {
        return scheme("a", function(list(optional("a")), list("a")));
    }

    /**
     * Returns the implementation of this primitive function.
     * @return a function that filters and extracts Just values from a list
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<java.util.List<Optional<Term>>, Term>) optionals -> Terms.list(apply(optionals)), hydra.extract.Core.listOf(x -> hydra.extract.Core.optionalTerm(t -> Either.right(t), graph, x), graph, args.get(0)));
    }

    /**
     * Filters and extracts Just values from a list of optionals.
     * @param <X> the element type
     * @param opt the list of optional values
     * @return a list containing only the present values
     */
    public static <X> List<X> apply(List<Optional<X>> opt) {
        ConsList<X> reversed = ConsList.empty();
        for (Optional<X> x : opt) {
            if (x.isGiven()) {
                reversed = ConsList.cons(x.fromGiven(), reversed);
            }
        }
        return reversed.reverse();
    }
}
