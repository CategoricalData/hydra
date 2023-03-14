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

import static hydra.Flows.*;
import static hydra.dsl.Types.*;

public class Cat<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.cat");
    }

    @Override
    public Type<A> type() {
        return function(list(string()), string());
    }

    @Override
    protected Function<List<Term<A>>, Flow<Graph<A>, Term<A>>> implementation() {
        return args -> map(Expect.list(Expect::string, args.get(0)),
            strings -> Terms.string(apply(strings)));
    }

    public static String apply(List<String> args) {
        return String.join("", args);
    }
}
