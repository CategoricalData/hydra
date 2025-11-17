package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Groups consecutive equal elements.
 */
public class Group extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.group");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list("a"), list(list("a"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.list(Flows::pure, args.get(0)),
            (Function<List<Term>, Term>) lst -> {
                // Simplified implementation
                List<List<Term>> groups = new ArrayList<>();
                if (!lst.isEmpty()) {
                    List<Term> currentGroup = new ArrayList<>();
                    currentGroup.add(lst.get(0));
                    for (int i = 1; i < lst.size(); i++) {
                        if (lst.get(i).equals(lst.get(i - 1))) {
                            currentGroup.add(lst.get(i));
                        } else {
                            groups.add(currentGroup);
                            currentGroup = new ArrayList<>();
                            currentGroup.add(lst.get(i));
                        }
                    }
                    groups.add(currentGroup);
                }
                return Terms.list(groups.stream().map(Terms::list).collect(java.util.stream.Collectors.toList()));
            });
    }

    /**
     * Groups consecutive equal elements.
     * @param <X> the element type
     * @param lst the list to group
     * @return the list of groups of consecutive equal elements
     */
    public static <X> List<List<X>> apply(List<X> lst) {
        List<List<X>> groups = new ArrayList<>();
        if (lst.isEmpty()) {
            return groups;
        }
        List<X> currentGroup = new ArrayList<>();
        currentGroup.add(lst.get(0));
        for (int i = 1; i < lst.size(); i++) {
            X current = lst.get(i);
            X previous = lst.get(i - 1);
            if ((current == null && previous == null)
                    || (current != null && current.equals(previous))) {
                currentGroup.add(current);
            } else {
                groups.add(currentGroup);
                currentGroup = new ArrayList<>();
                currentGroup.add(current);
            }
        }
        groups.add(currentGroup);
        return groups;
    }
}
