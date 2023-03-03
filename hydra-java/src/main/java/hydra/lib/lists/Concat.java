package hydra.lib.lists;

import hydra.core.Name;
import hydra.util.PrimitiveFunction;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class Concat<A> extends PrimitiveFunction<A> {
    public Name name() {
        return new Name("hydra/lib/lists.concat");
    }

    public static <X> List<X> apply(List<List<X>> sublists) {
        return sublists.stream().flatMap(Collection::stream).collect(Collectors.toList());
    }
}
