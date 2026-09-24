package hydra.core.overlay.java.lib.regex;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.Optional;

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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(
            hydra.core.extract.Core.string(graph, args.get(0)),
            pat -> hydra.core.overlay.java.lib.eithers.Bind.apply(
                hydra.core.extract.Core.string(graph, args.get(1)),
                repl -> hydra.core.overlay.java.lib.eithers.Map.apply(
                    input -> Terms.string(apply(pat, repl, input)),
                    hydra.core.extract.Core.string(graph, args.get(2)))));
    }

    public static Function<String, Function<String, String>> apply(String pattern) {
        return replacement -> input -> apply(pattern, replacement, input);
    }

    public static String apply(String pattern, String replacement, String input) {
        Optional<String> native_ = Native.toNative(pattern);
        if (native_.isNone()) {
            return input;
        }
        Matcher m = Pattern.compile(native_.fromGiven()).matcher(input);
        return m.replaceAll(Matcher.quoteReplacement(replacement));
    }
}
