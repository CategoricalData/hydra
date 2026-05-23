package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.pairs} library. */
public final class Pairs {
    private Pairs() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.pairs." + name);
    }

    public static <A> TTerm<A> bimap(TTerm<?> f, TTerm<?> g, TTerm<?> p) { return Phantoms.apply(prim("bimap"), f, g, p); }
    public static <A> TTerm<A> first(TTerm<?> pair) { return Phantoms.apply(prim("first"), pair); }
    public static <A> TTerm<A> second(TTerm<?> pair) { return Phantoms.apply(prim("second"), pair); }
}
