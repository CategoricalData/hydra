package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.math.Add;
import hydra.lib.math.Div;
import hydra.lib.math.Mod;
import hydra.lib.math.Mul;
import hydra.lib.math.Neg;
import hydra.lib.math.Rem;
import hydra.lib.math.Sub;

public interface Math {
    static Term add() {
        return new Add().term();
    }

    static Term div() {
        return new Div().term();
    }

    static Term mod() {
        return new Mod().term();
    }

    static Term mul() {
        return new Mul().term();
    }

    static Term neg() {
        return new Neg().term();
    }

    static Term rem() {
        return new Rem().term();
    }

    static Term sub() {
        return new Sub().term();
    }
}
