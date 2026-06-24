package hydra.overlay.java.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.int32;
import static hydra.overlay.java.dsl.Types.scheme;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;


/**
 * Performs addition on two numbers.
 */
public class Add extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return hydra.lib.Math_.add().name;
    }

    /**
     * Gets the type scheme for this function.
     * @return the type scheme
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int32(), int32(), int32()));
    }

    /**
     * Provides the implementation of this function.
     * @return a function that maps terms to a flow of terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(hydra.extract.Core.int32(graph, args.get(0)), arg0 -> hydra.overlay.java.lib.eithers.Map.apply(arg1 -> Terms.int32(apply(arg0, arg1)), hydra.extract.Core.int32(graph, args.get(1))));
    }

    /**
     * Adds two numbers.
     * @param augend the augend
     * @return the sum
     */
    public static Function<Integer, Integer> apply(Integer augend) {
        return (addend) -> apply(augend, addend);
    }

    /**
     * Adds two numbers.
     * @param augend the augend
     * @param addend the addend
     * @return the sum
     */
    public static Integer apply(Integer augend, Integer addend) {
        return (augend + addend);
    }
}
