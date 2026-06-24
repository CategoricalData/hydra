package hydra.overlay.java.lib.regex;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.regex.Pattern;

import static hydra.overlay.java.dsl.Types.boolean_;
import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Checks whether an entire string matches a regex pattern.
 */
public class Matches extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Regex.matches().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), boolean_()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(
            hydra.extract.Core.string(graph, args.get(0)),
            pat -> hydra.overlay.java.lib.eithers.Map.apply(
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
