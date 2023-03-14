package hydra.lib.strings;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import static hydra.dsl.Types.*;

public class Length<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/strings.length");
    }

    @Override
    public Type<A> type() {
        return function(string(), int32());
    }

    public static int apply(String s) {
        return s.length();
    }
}
