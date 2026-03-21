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
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import static hydra.dsl.Types.uint8;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


/**
 * Primitive function which converts a uint8 (8-bit unsigned integer) to its string representation.
 */
public class ShowUint8 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showUint8"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showUint8");
    }

    /**
     * Returns the type scheme for this function: uint8 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(uint8(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts uint8 terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<Short, Term>) s -> Terms.string(apply(s)), hydra.extract.core.Core.uint8(cx, graph, args.get(0)));
    }

    /**
     * Converts a Short (used to represent 8-bit unsigned integer) to its string representation.
     * @param value the Short value to convert
     * @return the string representation of the value
     */
    public static String apply(Short value) {
        return Short.toString(value);
    }
}
