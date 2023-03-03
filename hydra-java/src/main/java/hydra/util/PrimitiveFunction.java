package hydra.util;

import hydra.core.Name;
import hydra.core.Term;
import hydra.dsl.Terms;

public abstract class PrimitiveFunction<A> {
    public abstract Name name();

    public Term<A> term() {
        return Terms.primitive(name()); 
    }
}
