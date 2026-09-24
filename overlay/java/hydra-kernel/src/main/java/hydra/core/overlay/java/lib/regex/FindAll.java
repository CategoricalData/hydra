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
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;
import hydra.core.overlay.java.util.Optional;

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
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(
            hydra.core.extract.Core.string(graph, args.get(0)),
            pat -> hydra.core.overlay.java.lib.eithers.Map.apply(
                input -> Terms.listOfStrings(apply(pat, input)),
                hydra.core.extract.Core.string(graph, args.get(1))));
    }

    public static Function<String, List<String>> apply(String pattern) {
        return input -> apply(pattern, input);
    }

    public static List<String> apply(String pattern, String input) {
        Optional<String> native_ = Native.toNative(pattern);
        if (native_.isNone()) {
            return ConsList.<String>empty();
        }
        Matcher m = Pattern.compile(native_.fromGiven()).matcher(input);
        ConsList<String> reversed = ConsList.empty();
        while (m.find()) {
            reversed = ConsList.cons(m.group(), reversed);
        }
        return reversed.reverse();
    }
}
