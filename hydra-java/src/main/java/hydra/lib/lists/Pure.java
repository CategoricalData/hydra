package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.Collections;
import static hydra.dsl.Types.*;

public class Pure<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.pure");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function("x", list("x")));
    }

    public static <X> List<X> apply(X single) {
        return Collections.singletonList(single);
    }
}
