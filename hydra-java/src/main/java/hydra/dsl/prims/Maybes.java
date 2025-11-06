package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.maybes.Apply;
import hydra.lib.maybes.Bind;
import hydra.lib.maybes.Map;
import hydra.lib.maybes.Pure;

public interface Maybes {
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
