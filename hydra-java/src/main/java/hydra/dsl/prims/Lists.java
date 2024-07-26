package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.lists.Apply;
import hydra.lib.lists.Bind;
import hydra.lib.lists.Concat;
import hydra.lib.lists.Head;
import hydra.lib.lists.Intercalate;
import hydra.lib.lists.Intersperse;
import hydra.lib.lists.Last;
import hydra.lib.lists.Length;
import hydra.lib.lists.Map;
import hydra.lib.lists.Pure;

public interface Lists {
    static Term apply() {
        return new Apply().term();
    }

    static Term bind() {
        return new Bind().term();
    }

    static Term concat() {
        return new Concat().term();
    }

    static Term head() {
        return new Head().term();
    }

    static Term intercalate() {
        return new Intercalate().term();
    }

    static Term intersperse() {
        return new Intersperse().term();
    }

    static Term last() {
        return new Last().term();
    }

    static Term length() {
        return new Length().term();
    }

    static Term map() {
        return new Map().term();
    }

    static Term pure() {
        return new Pure().term();
    }
}
