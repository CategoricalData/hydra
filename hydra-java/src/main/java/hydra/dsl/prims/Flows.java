package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.flows.Apply;
import hydra.lib.flows.Bind;
import hydra.lib.flows.Map;
import hydra.lib.flows.Pure;


public interface Flows {
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
