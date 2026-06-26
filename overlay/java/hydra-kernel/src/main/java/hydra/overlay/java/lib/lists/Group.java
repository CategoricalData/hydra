package hydra.overlay.java.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.overlay.java.dsl.Types;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.schemeEq;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;


/**
 * Groups consecutive equal elements.
 */
public class Group extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Lists.group().name;
    }

    @Override
    public TypeScheme type() {
        return schemeEq("a", function(list("a"), list(list("a"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Map.apply((Function<List<Term>, Term>) lst -> {
                List<List<Term>> groups = apply(lst);
                ConsList<Term> reversed = ConsList.empty();
                for (List<Term> g : groups) {
                    reversed = ConsList.cons(Terms.list(g), reversed);
                }
                return Terms.list(reversed.reverse());
            }, hydra.extract.Core.list(graph, args.get(0)));
    }

    /**
     * Groups consecutive equal elements.
     * @param <X> the element type
     * @param lst the list to group
     * @return the list of groups of consecutive equal elements
     */
    public static <X> List<List<X>> apply(List<X> lst) {
        ConsList<ConsList<X>> reversedGroups = ConsList.empty();
        ConsList<X> reversedCurrent = ConsList.empty();
        boolean started = false;
        X previous = null;
        for (X current : lst) {
            if (!started) {
                reversedCurrent = ConsList.cons(current, reversedCurrent);
                started = true;
            } else if (Objects.equals(current, previous)) {
                reversedCurrent = ConsList.cons(current, reversedCurrent);
            } else {
                reversedGroups = ConsList.cons(reversedCurrent.reverse(), reversedGroups);
                reversedCurrent = ConsList.cons(current, ConsList.empty());
            }
            previous = current;
        }
        if (started) {
            reversedGroups = ConsList.cons(reversedCurrent.reverse(), reversedGroups);
        }
        ConsList<List<X>> result = ConsList.empty();
        for (ConsList<X> g : reversedGroups) {
            result = ConsList.cons(g, result);
        }
        return result;
    }
}
