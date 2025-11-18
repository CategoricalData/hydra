package hydra.lib.literals;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.float32;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;


/**
 * Primitive function which parses a string into a float32 (32-bit floating-point).
 * Returns an optional value that is empty if the string cannot be parsed.
 */
public class ReadFloat32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readFloat32"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readFloat32");
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional float32.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(float32())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional float32 terms
     */
    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.map(Expect.string(args.get(0)),
            (Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::float32)));
    }

    /**
     * Attempts to parse a string into a Float (32-bit).
     * @param str the string to parse
     * @return an Opt containing the parsed Float, or empty if parsing fails
     */
    public static Maybe<Float> apply(String str) {
        try {
            return Maybe.just(Float.parseFloat(str));
        } catch (NumberFormatException e) {
            return Maybe.nothing();
        }
    }
}
