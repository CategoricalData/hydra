package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.maps.Empty;


public interface Maps {
    static Term add() {
        return new Empty().term();
    }
}
