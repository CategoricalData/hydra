package hydra.lib.strings;

import hydra.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.string;

public class ToList<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.toList");
    }

    @Override
    public Type<A> type() {
        return function(string(), list(int32()));
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)), (Function<String, Term<A>>) s -> {
            List<Integer> list = apply(s);
            List<Term<A>> terms = new ArrayList<>(list.size());
            for (Integer i : list) {
                terms.add(Terms.int32(i));
            }
            return Terms.list(terms);
        });
    }

    public static List<Integer> apply(String s) {
        List<Integer> list = new ArrayList<>(s.length());
        for (char c : s.toCharArray()) {
            list.add((int) c);
        }
        return list;
    }
}
