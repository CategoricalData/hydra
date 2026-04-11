package hydra.lib.regex;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Replaces the first occurrence of a regex pattern with a replacement string.
 */
public class Replace extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.regex.replace");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), string(), string()));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.string(graph, args.get(0)),
            pat -> hydra.lib.eithers.Bind.apply(
                hydra.extract.Core.string(graph, args.get(1)),
                repl -> hydra.lib.eithers.Map.apply(
                    input -> Terms.string(apply(pat, repl, input)),
                    hydra.extract.Core.string(graph, args.get(2)))));
    }

    public static Function<String, Function<String, String>> apply(String pattern) {
        return replacement -> input -> apply(pattern, replacement, input);
    }

    public static String apply(String pattern, String replacement, String input) {
        Matcher m = Pattern.compile(pattern).matcher(input);
        return m.replaceFirst(Matcher.quoteReplacement(replacement));
    }
}
