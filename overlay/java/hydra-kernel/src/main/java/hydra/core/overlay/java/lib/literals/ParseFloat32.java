package hydra.core.overlay.java.lib.literals;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.float32;
import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;


/**
 * Primitive function which parses a string into a float32 (32-bit floating-point).
 * Returns an optional value that is empty if the string cannot be parsed.
 */
public class ParseFloat32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.core.lib.literals.parseFloat32"
     */
    public Name name() {
        return hydra.core.lib.Literals.parseFloat32().name;
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
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::float32)), hydra.core.extract.Model.string(graph, args.get(0)));
    }

    /**
     * Attempts to parse a string into a Float (32-bit).
     * @param str the string to parse
     * @return an Opt containing the parsed Float, or empty if parsing fails
     */
    public static Optional<Float> apply(String str) {
        try {
            return Optional.given(Float.parseFloat(str));
        } catch (NumberFormatException e) {
            return Optional.none();
        }
    }
}
