package hydra.lib.regex;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.regex.Pattern;

import static hydra.dsl.Types.boolean_;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Checks whether an entire string matches a regex pattern.
 */
public class Matches extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.regex.matches");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.string(graph, args.get(0)),
            pat -> hydra.lib.eithers.Map.apply(
                input -> Terms.boolean_(apply(pat, input)),
                hydra.extract.Core.string(graph, args.get(1))));
    }

    public static Function<String, Boolean> apply(String pattern) {
        return input -> apply(pattern, input);
    }

    public static boolean apply(String pattern, String input) {
        return Pattern.matches(pattern, input);
    }
}
