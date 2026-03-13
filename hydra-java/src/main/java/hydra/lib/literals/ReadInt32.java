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

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.int32;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.error.Error_;
import hydra.util.Either;


/**
 * Primitive function which parses a string into an int32 (32-bit signed integer).
 * Returns an optional value that is empty if the string cannot be parsed.
 */
public class ReadInt32 extends PrimitiveFunction {
    /**
     * Returns the unique name identifying this primitive function.
     * @return the function name "hydra.lib.literals.readInt32"
     */
    public Name name() {
        return new Name("hydra.lib.literals.readInt32");
    }

    /**
     * Returns the type scheme for this function: string -&gt; optional int32.
     * @return the type scheme representing the function signature
     */
    @Override
    public TypeScheme type() {
        return scheme(function(string(), optional(int32())));
    }

    /**
     * Provides the implementation of this primitive function.
     * @return a function that parses string terms into optional int32 terms
     */
    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Map.apply((Function<String, Term>) s -> Terms.optional(apply(s).map(Terms::int32)), hydra.extract.core.Core.string(cx, graph, args.get(0)));
    }

    /**
     * Attempts to parse a string into an Integer (32-bit signed).
     * @param str the string to parse
     * @return an Opt containing the parsed Integer, or empty if parsing fails
     */
    public static Maybe<Integer> apply(String str) {
        try {
            return Maybe.just(Integer.parseInt(str));
        } catch (NumberFormatException e) {
            return Maybe.nothing();
        }
    }
}
