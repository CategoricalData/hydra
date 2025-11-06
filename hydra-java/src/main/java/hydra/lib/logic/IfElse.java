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

/**
 * Performs conditional branching based on a boolean condition.
 */
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

    /**
     * Returns a function that selects between two values based on the condition.
     * @param condition the boolean condition
     * @param <X> the type of the values
     * @return a function that takes two values and returns the selected one
     */
    public static <X> Function<X, Function<X, X>> apply(boolean condition) {
        return x -> apply(condition, x);
    }

    /**
     * Returns a function that selects between the given value and another value based on the condition.
     * @param condition the boolean condition
     * @param ifBranch the value to return if condition is true
     * @param <X> the type of the values
     * @return a function that takes the else value and returns the selected value
     */
    public static <X> Function<X, X> apply(boolean condition, X ifBranch) {
        return elseBranch -> apply(condition, ifBranch, elseBranch);
    }

    /**
     * Selects between two values based on a boolean condition.
     * @param condition the boolean condition
     * @param ifBranch the value to return if condition is true
     * @param elseBranch the value to return if condition is false
     * @param <X> the type of the values
     * @return ifBranch if condition is true, elseBranch otherwise
     */
    public static <X> X apply(boolean condition, X ifBranch, X elseBranch) {
        return condition ? ifBranch : elseBranch;
    }
}
