package hydra.lib.maybes;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Filters and extracts Just values.
 */
public class Cat extends PrimitiveFunction {
    /**
     * Returns the name of this primitive function.
     * @return the name "hydra.lib.maybes.cat"
     */
    public Name name() {
        return new Name("hydra.lib.maybes.cat");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<java.util.List<Maybe<Term>>, Term>) optionals -> Terms.list(apply(optionals)), hydra.extract.Core.listOf(cx, x -> hydra.extract.Core.maybeTerm(cx, t -> Either.right(t), graph, x), graph, args.get(0)));
    }

    /**
     * Filters and extracts Just values from a list of optionals.
     * @param <X> the element type
     * @param opt the list of optional values
     * @return a list containing only the present values
     */
    public static <X> List<X> apply(List<Maybe<X>> opt) {
        ArrayList<X> result = new ArrayList<>();
        for (Maybe<X> x : opt) {
            x.ifJust(result::add);
        }
        return result;
    }
}
