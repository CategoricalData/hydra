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
import static hydra.overlay.java.dsl.Types.list;
import static hydra.overlay.java.dsl.Types.scheme;
import static hydra.overlay.java.dsl.Types.string;
import hydra.errors.Error_;
import hydra.overlay.java.util.ConsList;
import hydra.overlay.java.util.Either;

/**
 * Finds all non-overlapping substrings matching a regex pattern.
 */
public class FindAll extends PrimitiveFunction {
    public Name name() {
        return hydra.lib.Regex.findAll().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), list(string())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.overlay.java.lib.eithers.Bind.apply(
            hydra.extract.Core.string(graph, args.get(0)),
            pat -> hydra.overlay.java.lib.eithers.Map.apply(
                input -> Terms.listOfStrings(apply(pat, input)),
                hydra.extract.Core.string(graph, args.get(1))));
    }

    public static Function<String, List<String>> apply(String pattern) {
        return input -> apply(pattern, input);
    }

    public static List<String> apply(String pattern, String input) {
        Matcher m = Pattern.compile(pattern).matcher(input);
        ConsList<String> reversed = ConsList.empty();
        while (m.find()) {
            reversed = ConsList.cons(m.group(), reversed);
        }
        return reversed.reverse();
    }
}
