package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.equality} library. */
public final class Equality {
    private Equality() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.equality." + name);
    }

    public static <A> TTerm<A> compare(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("compare"), x, y); }
    public static <A> TTerm<A> equal(TTerm<?> a, TTerm<?> b) { return Phantoms.apply(prim("equal"), a, b); }
    public static <A> TTerm<A> gt(TTerm<?> a, TTerm<?> b) { return Phantoms.apply(prim("gt"), a, b); }
    public static <A> TTerm<A> gte(TTerm<?> a, TTerm<?> b) { return Phantoms.apply(prim("gte"), a, b); }
    public static <A> TTerm<A> identity(TTerm<?> a) { return Phantoms.apply(prim("identity"), a); }
    public static <A> TTerm<A> lt(TTerm<?> a, TTerm<?> b) { return Phantoms.apply(prim("lt"), a, b); }
    public static <A> TTerm<A> lte(TTerm<?> a, TTerm<?> b) { return Phantoms.apply(prim("lte"), a, b); }
    public static <A> TTerm<A> max_(TTerm<?> a, TTerm<?> b) { return Phantoms.apply(prim("max"), a, b); }
    public static <A> TTerm<A> min_(TTerm<?> a, TTerm<?> b) { return Phantoms.apply(prim("min"), a, b); }
}
