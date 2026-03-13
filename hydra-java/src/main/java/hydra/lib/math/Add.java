package hydra.lib.math;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Performs addition on two numbers.
 */
public class Add extends PrimitiveFunction {
    /**
     * Gets the name of this primitive function.
     * @return the function name
     */
    public Name name() {
        return new Name("hydra.lib.math.add");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.core.Core.int32(cx, graph, args.get(0)), arg0 -> hydra.lib.eithers.Map.apply(arg1 -> Terms.int32(apply(arg0, arg1)), hydra.extract.core.Core.int32(cx, graph, args.get(1))));
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
