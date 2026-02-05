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
import java.util.function.Supplier;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.var;

/**
 * Performs conditional branching based on a boolean condition.
 */
public class IfElse extends PrimitiveFunction {
    /**
     * Returns the fully qualified name of this primitive function.
     * @return the name "hydra.lib.logic.ifElse"
     */
    public Name name() {
        return new Name("hydra.lib.logic.ifElse");
    }

    /**
     * Returns the type scheme for this function.
     * @return a polymorphic type scheme representing a function that takes two values of type 'a',
     *         a boolean condition, and returns a value of type 'a'
     */
    @Override
    public TypeScheme type() {
        return scheme("a",
            function(boolean_(), var("a"), var("a"), var("a")));
    }

    /**
     * Returns the implementation of this primitive function as a Flow computation.
     * @return a function that takes a list of terms and returns a Flow producing the selected value
     */
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

    /**
     * Selects between two lazily-evaluated branches based on a boolean condition.
     * Only the chosen branch's supplier is invoked, providing short-circuit evaluation
     * semantics matching Haskell's lazy ifElse.
     */
    public static <X> X lazy(boolean condition, Supplier<X> ifBranch, Supplier<X> elseBranch) {
        return condition ? ifBranch.get() : elseBranch.get();
    }
}
