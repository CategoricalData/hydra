package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.equality} library. */
public final class Equality {
    private Equality() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.equality." + name);
    }

    public static <A> TypedTerm<A> compare(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("compare"), x, y); }
    public static <A> TypedTerm<A> equal(TypedTerm<?> a, TypedTerm<?> b) { return Phantoms.apply(prim("equal"), a, b); }
    public static <A> TypedTerm<A> gt(TypedTerm<?> a, TypedTerm<?> b) { return Phantoms.apply(prim("gt"), a, b); }
    public static <A> TypedTerm<A> gte(TypedTerm<?> a, TypedTerm<?> b) { return Phantoms.apply(prim("gte"), a, b); }
    public static <A> TypedTerm<A> identity(TypedTerm<?> a) { return Phantoms.apply(prim("identity"), a); }
    public static <A> TypedTerm<A> lt(TypedTerm<?> a, TypedTerm<?> b) { return Phantoms.apply(prim("lt"), a, b); }
    public static <A> TypedTerm<A> lte(TypedTerm<?> a, TypedTerm<?> b) { return Phantoms.apply(prim("lte"), a, b); }
    public static <A> TypedTerm<A> max_(TypedTerm<?> a, TypedTerm<?> b) { return Phantoms.apply(prim("max"), a, b); }
    public static <A> TypedTerm<A> min_(TypedTerm<?> a, TypedTerm<?> b) { return Phantoms.apply(prim("min"), a, b); }
}
