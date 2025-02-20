package hydra.lib.logic;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;


public class IfElse extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.logic.ifElse");
    }

    @Override
    public TypeScheme type() {
        return scheme("a",
            function(var("a"), var("a"), boolean_(), var("a")));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.boolean_(args.get(0)), b -> IfElse.apply(b, args.get(1), args.get(2)));
    }

    public static <X> Function<X, Function<X, X>> apply(boolean condition) {
        return x -> apply(condition, x);
    }

    public static <X> Function<X, X> apply(boolean condition, X ifBranch) {
        return elseBranch -> apply(condition, ifBranch, elseBranch);
    }

    public static <X> X apply(boolean condition, X ifBranch, X elseBranch) {
        return condition ? ifBranch : elseBranch;
    }
}
