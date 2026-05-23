package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.logic} library. */
public final class Logic {
    private Logic() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.logic." + name);
    }

    public static <A> TTerm<A> and_(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("and"), x, y); }
    public static <A> TTerm<A> ands(TTerm<?> terms) { return Phantoms.apply(prim("ands"), terms); }
    public static <A> TTerm<A> ifElse(TTerm<?> b, TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("ifElse"), b, x, y); }
    public static <A> TTerm<A> not_(TTerm<?> x) { return Phantoms.apply(prim("not"), x); }
    public static <A> TTerm<A> or_(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("or"), x, y); }
    public static <A> TTerm<A> ors(TTerm<?> terms) { return Phantoms.apply(prim("ors"), terms); }
}
