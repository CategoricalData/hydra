package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.literals.ShowInt32;
import hydra.lib.literals.ShowString;


public interface Literals {
    static Term showInt32() {
        return new ShowInt32().term();
    }

    static Term showString() {
        return new ShowString().term();
    }
}
