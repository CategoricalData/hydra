package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.lists.*;

public interface Lists {
    static <M> Term<M> apply() {
        return new Apply().term();
    }

    static <M> Term<M> bind() {
        return new Bind().term();
    }

    static <M> Term<M> concat() {
        return new Concat().term();
    }

    static <M> Term<M> head() {
        return new Head().term();
    }

    static <M> Term<M> intercalate() {
        return new Intercalate().term();
    }

    static <M> Term<M> intersperse() {
        return new Intersperse().term();
    }

    static <M> Term<M> last() {
        return new Last().term();
    }

    static <M> Term<M> length() {
        return new Length().term();
    }

    static <M> Term<M> map() {
        return new Map().term();
    }

    static <M> Term<M> pure() {
        return new Pure().term();
    }
}
