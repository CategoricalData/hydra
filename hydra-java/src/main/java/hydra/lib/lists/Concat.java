package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Type;
import hydra.tools.PrimitiveFunction;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import static hydra.dsl.Types.*;

public class Concat<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.concat");
    }

    @Override
    public Type<A> type() {
        return lambda("x", function(list(list("x")), list("x")));
    }

    public static <X> List<X> apply(List<List<X>> sublists) {
        return sublists.stream().flatMap(Collection::stream).collect(Collectors.toList());
    }
}
