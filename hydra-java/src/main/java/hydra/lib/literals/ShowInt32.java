package hydra.lib.literals;

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
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Primitive function which converts an int32 (32-bit signed integer) to its string representation.
 */
public class ShowInt32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showInt32"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showInt32");
    }

    /**
     * Returns the type scheme for this function: int32 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int32(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts int32 terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<Integer, Term>) s -> Terms.string(apply(s)), hydra.extract.core.Core.int32(cx, graph, args.get(0)));
    }

    /**
     * Converts an Integer (32-bit signed integer) value to its string representation.
     * @param value the Integer value to convert
     * @return the string representation of the value
     */
    public static String apply(Integer value) {
        return Integer.toString(value);
    }
}
