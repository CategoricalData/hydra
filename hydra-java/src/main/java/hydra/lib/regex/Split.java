package hydra.lib.regex;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Pattern;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import static hydra.dsl.Types.string;
import hydra.context.Context;
import hydra.errors.Error_;
import hydra.util.Either;

/**
 * Splits a string by a regex pattern.
 */
public class Split extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.regex.split");
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), list(string())));
    }

    @Override
    protected Function<List<Term>, Function<Context, Function<Graph, Either<Error_, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(
            hydra.extract.Core.string(graph, args.get(0)),
            pat -> hydra.lib.eithers.Map.apply(
                input -> Terms.listOfStrings(apply(pat, input)),
                hydra.extract.Core.string(graph, args.get(1))));
    }

    public static Function<String, List<String>> apply(String pattern) {
        return input -> apply(pattern, input);
    }

    public static List<String> apply(String pattern, String input) {
        // Java's split with -1 preserves trailing empty strings
        String[] parts = Pattern.compile(pattern).split(input, -1);
        ArrayList<String> results = new ArrayList<>();
        for (String part : parts) {
            results.add(part);
        }
        return results;
    }
}
