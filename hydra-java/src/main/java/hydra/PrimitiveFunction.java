package hydra;

import hydra.core.Name;
import hydra.core.Term;
import hydra.dsl.Terms;

public abstract class PrimitiveFunction<M> {
    public abstract Name name();

    public Term<M> term() {
        return Terms.primitive(name()); 
    }
}
