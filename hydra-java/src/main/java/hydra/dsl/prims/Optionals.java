package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.optionals.Apply;
import hydra.lib.optionals.Bind;
import hydra.lib.optionals.Map;
import hydra.lib.optionals.Pure;

public interface Optionals {
    static Term apply() {
        return new Apply().term();
    }

    static Term bind() {
        return new Bind().term();
    }

    static Term map() {
        return new Map().term();
    }

    static Term pure() {
        return new Pure().term();
    }
}
