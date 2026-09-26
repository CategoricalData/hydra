package hydra.core.overlay.java.lib.regex;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;
import hydra.core.overlay.java.util.Optional;

import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.optional;
import static hydra.core.overlay.java.dsl.Types.scheme;
import static hydra.core.overlay.java.dsl.Types.string;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.Either;

/**
 * Finds the first substring matching a regex pattern.
 */
public class Find extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Regex.find().name;
    }

    @Override
    public TypeScheme type() {
        return scheme(function(string(), string(), optional(string())));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(
            hydra.core.extract.Model.string(graph, args.get(0)),
            pat -> hydra.core.overlay.java.lib.eithers.Map.apply(
                input -> Terms.optional(apply(pat, input).map(Terms::string)),
                hydra.core.extract.Model.string(graph, args.get(1))));
    }

    public static Function<String, Optional<String>> apply(String pattern) {
        return input -> apply(pattern, input);
    }

    public static Optional<String> apply(String pattern, String input) {
        Optional<String> native_ = Native.toNative(pattern);
        if (native_.isNone()) {
            return Optional.none();
        }
        Matcher m = Pattern.compile(native_.fromGiven()).matcher(input);
        if (m.find()) {
            return Optional.given(m.group());
        }
        return Optional.none();
    }
}
