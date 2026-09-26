package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.int16;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Primitive function which converts an int16 (16-bit signed integer) to its string representation.
 */
public class PrintInt16 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.printInt16"
     */
    public Name name() {
        return hydra.core.lib.Literals.printInt16().name;
    }

    /**
     * Returns the type scheme for this function: int16 -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(int16(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts int16 terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<Short, Term>) s -> Terms.string(apply(s)), hydra.core.extract.Model.int16(graph, args.get(0)));
    }

    /**
     * Converts a Short (16-bit signed integer) value to its string representation.
     * @param value the Short value to convert
     * @return the string representation of the value
     */
    public static String apply(Short value) {
        return Short.toString(value);
    }
}
