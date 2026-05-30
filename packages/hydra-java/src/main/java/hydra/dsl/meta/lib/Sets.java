package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.sets} library. */
public final class Sets {
    private Sets() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.sets." + name);
    }

    public static <A> TypedTerm<A> delete(TypedTerm<?> x, TypedTerm<?> s) { return Phantoms.apply(prim("delete"), x, s); }
    public static <A> TypedTerm<A> difference(TypedTerm<?> s1, TypedTerm<?> s2) { return Phantoms.apply(prim("difference"), s1, s2); }
    public static <A> TypedTerm<A> empty() { return prim("empty"); }
    public static <A> TypedTerm<A> fromList(TypedTerm<?> xs) { return Phantoms.apply(prim("fromList"), xs); }
    public static <A> TypedTerm<A> insert(TypedTerm<?> x, TypedTerm<?> s) { return Phantoms.apply(prim("insert"), x, s); }
    public static <A> TypedTerm<A> intersection(TypedTerm<?> s1, TypedTerm<?> s2) { return Phantoms.apply(prim("intersection"), s1, s2); }
    public static <A> TypedTerm<A> map(TypedTerm<?> f, TypedTerm<?> s) { return Phantoms.apply(prim("map"), f, s); }
    public static <A> TypedTerm<A> member(TypedTerm<?> x, TypedTerm<?> s) { return Phantoms.apply(prim("member"), x, s); }
    public static <A> TypedTerm<A> null_(TypedTerm<?> s) { return Phantoms.apply(prim("null"), s); }
    public static <A> TypedTerm<A> singleton(TypedTerm<?> x) { return Phantoms.apply(prim("singleton"), x); }
    public static <A> TypedTerm<A> size(TypedTerm<?> s) { return Phantoms.apply(prim("size"), s); }
    public static <A> TypedTerm<A> toList(TypedTerm<?> s) { return Phantoms.apply(prim("toList"), s); }
    public static <A> TypedTerm<A> union(TypedTerm<?> s1, TypedTerm<?> s2) { return Phantoms.apply(prim("union"), s1, s2); }
    public static <A> TypedTerm<A> unions(TypedTerm<?> sets) { return Phantoms.apply(prim("unions"), sets); }
}
