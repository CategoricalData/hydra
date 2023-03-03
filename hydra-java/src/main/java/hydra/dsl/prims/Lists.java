package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.lists.*;

public interface Lists {
    static <A> Term<A> apply() {
        return new Apply().term();
    }

    static <A> Term<A> bind() {
        return new Bind().term();
    }

    static <A> Term<A> concat() {
        return new Concat().term();
    }

    static <A> Term<A> head() {
        return new Head().term();
    }

    static <A> Term<A> intercalate() {
        return new Intercalate().term();
    }

    static <A> Term<A> intersperse() {
        return new Intersperse().term();
    }

    static <A> Term<A> last() {
        return new Last().term();
    }

    static <A> Term<A> length() {
        return new Length().term();
    }

    static <A> Term<A> map() {
        return new Map().term();
    }

    static <A> Term<A> pure() {
        return new Pure().term();
    }
}
