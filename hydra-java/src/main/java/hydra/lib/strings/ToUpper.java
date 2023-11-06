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
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.string;


public class ToUpper<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.toUpper");
    }

    @Override
    public Type<A> type() {
        return function(string(), string());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)), s -> Terms.string(apply(s)));
    }

    public static String apply(String lower) {
        // TODO: Java's built-in behavior may not agree with that of Haskell or other host languages
        return lower.toUpperCase();
    }
}
