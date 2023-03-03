package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.strings.*;

public interface Strings {
    static <A> Term<A> cat() {
        return new Cat().term();
    }

    static <A> Term<A> length() {
        return new Length().term();
    }

    static <A> Term<A> splitOn() {
        return new SplitOn().term();
    }

    static <A> Term<A> toLower() {
        return new ToLower().term();
    }

    static <A> Term<A> toUpper() {
        return new ToUpper().term();
    }
}
