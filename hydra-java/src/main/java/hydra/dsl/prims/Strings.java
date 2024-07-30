package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.strings.Length;
import hydra.lib.strings.Cat;
import hydra.lib.strings.SplitOn;
import hydra.lib.strings.ToLower;
import hydra.lib.strings.ToUpper;

public interface Strings {
    static Term cat() {
        return new Cat().term();
    }

    static Term length() {
        return new Length().term();
    }

    static Term splitOn() {
        return new SplitOn().term();
    }

    static Term toLower() {
        return new ToLower().term();
    }

    static Term toUpper() {
        return new ToUpper().term();
    }
}
