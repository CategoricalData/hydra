package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.pairs} library. */
public final class Pairs {
    private Pairs() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.pairs." + name);
    }

    public static <A> TypedTerm<A> bimap(TypedTerm<?> f, TypedTerm<?> g, TypedTerm<?> p) { return Phantoms.apply(prim("bimap"), f, g, p); }
    public static <A> TypedTerm<A> first(TypedTerm<?> pair) { return Phantoms.apply(prim("first"), pair); }
    public static <A> TypedTerm<A> second(TypedTerm<?> pair) { return Phantoms.apply(prim("second"), pair); }
}
