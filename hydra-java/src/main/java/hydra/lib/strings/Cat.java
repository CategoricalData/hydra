package hydra.lib.strings;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.List;

import static hydra.dsl.Types.*;

public class Cat<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.cat");
    }

    @Override
    public Type<A> type() {
        return function(list(string()), string());
    }

    public static String apply(List<String> args) {
        return String.join("", args);
    }
}
