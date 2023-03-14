package hydra.tools;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.dsl.Terms;
import java.util.List;


public abstract class PrimitiveFunction<A> {
    public abstract Name name();

    public Term<A> term() {
        return Terms.primitive(name());
    }

    public abstract Type<A> type();
}
