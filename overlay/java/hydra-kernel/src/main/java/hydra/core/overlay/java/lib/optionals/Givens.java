package hydra.core.overlay.java.lib.optionals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Filters and extracts Just values.
 */
public class Givens extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.core.lib.optionals.givens"
     */
    public Name name() {
        return hydra.core.lib.Optionals.givens().name;
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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<java.util.List<Optional<Term>>, Term>) optionals -> Terms.list(apply(optionals)), hydra.core.extract.Model.listOf(x -> hydra.core.extract.Model.optionalTerm(t -> Either.right(t), graph, x), graph, args.get(0)));
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
