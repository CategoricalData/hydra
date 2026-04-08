package hydra.lib.regex;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import hydra.util.Maybe;

import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.optional;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Finds the first substring matching a regex pattern.
 */
public class Find extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.regex.find");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), optional(string())));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.string(graph, args.get(0)),
            pat -> hydra.lib.eithers.Map.apply(
                input -> Terms.optional(apply(pat, input).map(Terms::string)),
                hydra.extract.Core.string(graph, args.get(1))));
    }

    public static Function<String, Maybe<String>> apply(String pattern) {
        return input -> apply(pattern, input);
    }

    public static Maybe<String> apply(String pattern, String input) {
        Matcher m = Pattern.compile(pattern).matcher(input);
        if (m.find()) {
            return Maybe.just(m.group());
        }
        return Maybe.nothing();
    }
}
