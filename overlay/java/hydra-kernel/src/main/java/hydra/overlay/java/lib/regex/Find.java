package hydra.overlay.java.lib.regex;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;
import hydra.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.optional;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Finds the first substring matching a regex pattern.
 */
public class Find extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Regex.find().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), optional(string())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(
            hydra.extract.Core.string(graph, args.get(0)),
            pat -> hydra.overlay.java.lib.eithers.Map.apply(
                input -> Terms.optional(apply(pat, input).map(Terms::string)),
                hydra.extract.Core.string(graph, args.get(1))));
    }

    public static Function<String, Optional<String>> apply(String pattern) {
        return input -> apply(pattern, input);
    }

    public static Optional<String> apply(String pattern, String input) {
        Matcher m = Pattern.compile(pattern).matcher(input);
        if (m.find()) {
            return Optional.given(m.group());
        }
        return Optional.none();
    }
}
