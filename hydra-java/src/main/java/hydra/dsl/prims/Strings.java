package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.strings.*;

public interface Strings {
    static <M> Term<M> cat() {
        return new Cat().term();
    }

    static <M> Term<M> length() {
        return new Length().term();
    }

    static <M> Term<M> splitOn() {
        return new SplitOn().term();
    }

    static <M> Term<M> toLower() {
        return new ToLower().term();
    }

    static <M> Term<M> toUpper() {
        return new ToUpper().term();
    }
}
