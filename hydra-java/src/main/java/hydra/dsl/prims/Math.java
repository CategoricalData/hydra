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
    static <A> Term<A> add() {
        return new Add().term();
    }

    static <A> Term<A> div() {
        return new Div().term();
    }

    static <A> Term<A> mod() {
        return new Mod().term();
    }

    static <A> Term<A> mul() {
        return new Mul().term();
    }

    static <A> Term<A> neg() {
        return new Neg().term();
    }

    static <A> Term<A> rem() {
        return new Rem().term();
    }

    static <A> Term<A> sub() {
        return new Sub().term();
    }
}
