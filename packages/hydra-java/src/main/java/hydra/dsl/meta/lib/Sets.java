package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.sets} library. */
public final class Sets {
    private Sets() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.sets." + name);
    }

    public static <A> TTerm<A> delete(TTerm<?> x, TTerm<?> s) { return Phantoms.apply(prim("delete"), x, s); }
    public static <A> TTerm<A> difference(TTerm<?> s1, TTerm<?> s2) { return Phantoms.apply(prim("difference"), s1, s2); }
    public static <A> TTerm<A> empty() { return prim("empty"); }
    public static <A> TTerm<A> fromList(TTerm<?> xs) { return Phantoms.apply(prim("fromList"), xs); }
    public static <A> TTerm<A> insert(TTerm<?> x, TTerm<?> s) { return Phantoms.apply(prim("insert"), x, s); }
    public static <A> TTerm<A> intersection(TTerm<?> s1, TTerm<?> s2) { return Phantoms.apply(prim("intersection"), s1, s2); }
    public static <A> TTerm<A> map(TTerm<?> f, TTerm<?> s) { return Phantoms.apply(prim("map"), f, s); }
    public static <A> TTerm<A> member(TTerm<?> x, TTerm<?> s) { return Phantoms.apply(prim("member"), x, s); }
    public static <A> TTerm<A> null_(TTerm<?> s) { return Phantoms.apply(prim("null"), s); }
    public static <A> TTerm<A> singleton(TTerm<?> x) { return Phantoms.apply(prim("singleton"), x); }
    public static <A> TTerm<A> size(TTerm<?> s) { return Phantoms.apply(prim("size"), s); }
    public static <A> TTerm<A> toList(TTerm<?> s) { return Phantoms.apply(prim("toList"), s); }
    public static <A> TTerm<A> union(TTerm<?> s1, TTerm<?> s2) { return Phantoms.apply(prim("union"), s1, s2); }
    public static <A> TTerm<A> unions(TTerm<?> sets) { return Phantoms.apply(prim("unions"), sets); }
}
