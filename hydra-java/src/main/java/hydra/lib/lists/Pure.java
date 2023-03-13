package hydra.lib.lists;

import hydra.core.Name;
import hydra.tools.PrimitiveFunction;

import java.util.List;
import java.util.Collections;

public class Pure<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.pure");
    }

    public static <X> List<X> apply(X single) {
        return Collections.singletonList(single);
    }
}
