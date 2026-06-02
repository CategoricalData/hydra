package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.logic} library. */
public final class Logic {
    private Logic() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.logic." + name);
    }

    public static <A> TypedTerm<A> and_(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("and"), x, y); }
    public static <A> TypedTerm<A> ands(TypedTerm<?> terms) { return Phantoms.apply(prim("ands"), terms); }
    public static <A> TypedTerm<A> ifElse(TypedTerm<?> b, TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("ifElse"), b, x, y); }
    public static <A> TypedTerm<A> not_(TypedTerm<?> x) { return Phantoms.apply(prim("not"), x); }
    public static <A> TypedTerm<A> or_(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("or"), x, y); }
    public static <A> TypedTerm<A> ors(TypedTerm<?> terms) { return Phantoms.apply(prim("ors"), terms); }
}
