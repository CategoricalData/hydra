package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.boolean_;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Primitive function which converts a boolean to its string representation.
 * Returns "true" or "false".
 */
public class PrintBoolean extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.printBoolean"
     */
    public Name name() {
        return hydra.core.lib.Literals.printBoolean().name;
    }

    /**
     * Returns the type scheme for this function: boolean -&gt; string.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(boolean_(), string()));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that converts boolean terms to string terms
     */
    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<Boolean, Term>) b -> Terms.string(apply(b)), hydra.core.extract.Model.boolean_(graph, args.get(0)));
    }

    /**
     * Converts a Boolean value to its string representation.
     * @param value the Boolean value to convert
     * @return "true" if the value is true, "false" otherwise
     */
    public static String apply(Boolean value) {
        return value ? "true" : "false";
    }
}
