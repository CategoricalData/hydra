package hydra.dsl.prims;

import hydra.core.Term;
import hydra.lib.math.*;

public interface Math {
    static <M> Term<M> add() {
        return new Add().term();
    }

    static <M> Term<M> div() {
        return new Div().term();
    }

    static <M> Term<M> mod() {
        return new Mod().term();
    }

    static <M> Term<M> mul() {
        return new Mul().term();
    }

    static <M> Term<M> neg() {
        return new Neg().term();
    }

    static <M> Term<M> rem() {
        return new Rem().term();
    }

    static <M> Term<M> sub() {
        return new Sub().term();
    }
}
