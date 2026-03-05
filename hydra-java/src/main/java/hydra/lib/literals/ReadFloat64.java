package hydra.lib.literals;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.float64;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.OtherError;
import hydra.util.Either;


/**
 * Primitive function which parses a string into a float64 (64-bit floating-point).
 * Returns an optional value that is empty if the string cannot be parsed.
 */
public class ReadFloat64 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readFloat64"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readFloat64");
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional float64.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(float64())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional float64 terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<OtherError>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::float64)), hydra.extract.core.Core.string(cx, graph, args.get(0)));
    }

    /**
     * Attempts to parse a string into a Double (64-bit).
     * @param str the string to parse
     * @return an Opt containing the parsed Double, or empty if parsing fails
     */
    public static Maybe<Double> apply(String str) {
        try {
            return Maybe.just(Double.parseDouble(str));
        } catch (NumberFormatException e) {
            return Maybe.nothing();
        }
    }
}
