package hydra.lib.strings;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.Flows.map2;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.string;

public class Intercalate<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.intercalate");
    }

    @Override
    public Type<A> type() {
        return function(string(), list(string()), string());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> map2(
                Expect.string(args.get(0)),
                Expect.list(Expect::string, args.get(1)),
                (delim, strings) -> Terms.string(Intercalate.apply(delim, strings)));
    }

    public static Function<List<String>, String> apply(String delim) {
        return (strings) -> apply(delim, strings);
    }

    public static String apply(String delim, List<String> strings) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (String s : strings) {
            if (first) {
                first = false;
            } else {
                sb.append(delim);
            }
            sb.append(s);
        }
        return sb.toString();
    }
}
