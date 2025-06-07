package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.sets.Member;
import hydra.lib.sets.Empty;
import hydra.lib.sets.FromList;
import hydra.lib.sets.Insert;
import hydra.lib.sets.IsEmpty;
import hydra.lib.sets.Map;
import hydra.lib.sets.Delete;
import hydra.lib.sets.Singleton;
import hydra.lib.sets.Size;
import hydra.lib.sets.ToList;

public interface Sets {
    static Term contains() {
        return new Member().term();
    }

    static Term empty() {
        return new Empty().term();
    }

    static Term fromList() {
        return new FromList().term();
    }

    static Term insert() {
        return new Insert().term();
    }

    static Term isEmpty() {
        return new IsEmpty().term();
    }

    static Term map() {
        return new Map().term();
    }

    static Term remove() {
        return new Delete().term();
    }

    static Term singleton() {
        return new Singleton().term();
    }

    static Term size() {
        return new Size().term();
    }

    static Term toList() {
        return new ToList().term();
    }
}
