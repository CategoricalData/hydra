package hydra.overlay.java.lib.regex;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.overlay.java.dsl.Terms;
import hydra.graph.Graph;
import hydra.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static hydra.overlay.java.dsl.Types.function;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.Either;

/**
 * Replaces all non-overlapping occurrences of a regex pattern with a replacement string.
 */
public class ReplaceAll extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Regex.replaceAll().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), string(), string()));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(
            hydra.extract.Core.string(graph, args.get(0)),
            pat -> hydra.overlay.java.lib.eithers.Bind.apply(
                hydra.extract.Core.string(graph, args.get(1)),
                repl -> hydra.overlay.java.lib.eithers.Map.apply(
                    input -> Terms.string(apply(pat, repl, input)),
                    hydra.extract.Core.string(graph, args.get(2)))));
    }

    public static Function<String, Function<String, String>> apply(String pattern) {
        return replacement -> input -> apply(pattern, replacement, input);
    }

    public static String apply(String pattern, String replacement, String input) {
        Matcher m = Pattern.compile(pattern).matcher(input);
        return m.replaceAll(Matcher.quoteReplacement(replacement));
    }
}
