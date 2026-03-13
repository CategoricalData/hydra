package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Primitive function which converts a boolean to its string representation.
 * Returns "true" or "false".
 */
public class ShowBoolean extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.showBoolean"
     */
    public Name name() {
        return new Name("hydra.lib.literals.showBoolean");
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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<Boolean, Term>) b -> Terms.string(apply(b)), hydra.extract.core.Core.boolean_(cx, graph, args.get(0)));
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
