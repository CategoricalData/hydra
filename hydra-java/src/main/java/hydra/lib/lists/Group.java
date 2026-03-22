package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.ConsList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.schemeEq;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Groups consecutive equal elements.
 */
public class Group extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.group");
    }

    @Override
    public TypeScheme type() {
        return schemeEq("a", function(list("a"), list(list("a"))));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<ConsList<Term>, Term>) lst -> {
                // Convert to ArrayList for index-based access
                ArrayList<Term> items = lst.toArrayList();
                List<List<Term>> groups = new ArrayList<>();
                if (!items.isEmpty()) {
                    List<Term> currentGroup = new ArrayList<>();
                    currentGroup.add(items.get(0));
                    for (int i = 1; i < items.size(); i++) {
                        if (items.get(i).equals(items.get(i - 1))) {
                            currentGroup.add(items.get(i));
                        } else {
                            groups.add(currentGroup);
                            currentGroup = new ArrayList<>();
                            currentGroup.add(items.get(i));
                        }
                    }
                    groups.add(currentGroup);
                }
                return Terms.list(groups.stream().map(Terms::list).collect(java.util.stream.Collectors.toList()));
            }, hydra.extract.Core.list(cx, graph, args.get(0)));
    }

    /**
     * Groups consecutive equal elements.
     * @param <X> the element type
     * @param lst the list to group
     * @return the list of groups of consecutive equal elements
     */
    public static <X> ConsList<ConsList<X>> apply(ConsList<X> lst) {
        ArrayList<X> items = lst.toArrayList();
        List<ConsList<X>> groups = new ArrayList<>();
        if (items.isEmpty()) {
            return ConsList.empty();
        }
        List<X> currentGroup = new ArrayList<>();
        currentGroup.add(items.get(0));
        for (int i = 1; i < items.size(); i++) {
            X current = items.get(i);
            X previous = items.get(i - 1);
            if ((current == null && previous == null)
                    || (current != null && current.equals(previous))) {
                currentGroup.add(current);
            } else {
                groups.add(ConsList.fromList(currentGroup));
                currentGroup = new ArrayList<>();
                currentGroup.add(current);
            }
        }
        groups.add(ConsList.fromList(currentGroup));
        return ConsList.fromList(groups);
    }
}
