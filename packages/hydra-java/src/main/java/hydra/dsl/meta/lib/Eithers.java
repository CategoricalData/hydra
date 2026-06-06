package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.eithers} library. */
public final class Eithers {
    private Eithers() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.eithers." + name);
    }

    public static <A> TypedTerm<A> bimap(TypedTerm<?> f, TypedTerm<?> g, TypedTerm<?> e) { return Phantoms.apply(prim("bimap"), f, g, e); }
    public static <A> TypedTerm<A> bind(TypedTerm<?> e, TypedTerm<?> f) { return Phantoms.apply(prim("bind"), e, f); }
    public static <A> TypedTerm<A> either_(TypedTerm<?> f, TypedTerm<?> g, TypedTerm<?> e) { return Phantoms.apply(prim("either"), f, g, e); }
    public static <A> TypedTerm<A> foldl(TypedTerm<?> f, TypedTerm<?> init, TypedTerm<?> xs) { return Phantoms.apply(prim("foldl"), f, init, xs); }
    public static <A> TypedTerm<A> fromLeft(TypedTerm<?> defaultVal, TypedTerm<?> e) { return Phantoms.apply(prim("fromLeft"), defaultVal, e); }
    public static <A> TypedTerm<A> fromRight(TypedTerm<?> defaultVal, TypedTerm<?> e) { return Phantoms.apply(prim("fromRight"), defaultVal, e); }
    public static <A> TypedTerm<A> isLeft(TypedTerm<?> e) { return Phantoms.apply(prim("isLeft"), e); }
    public static <A> TypedTerm<A> isRight(TypedTerm<?> e) { return Phantoms.apply(prim("isRight"), e); }
    public static <A> TypedTerm<A> lefts(TypedTerm<?> eithers) { return Phantoms.apply(prim("lefts"), eithers); }
    public static <A> TypedTerm<A> map(TypedTerm<?> f, TypedTerm<?> e) { return Phantoms.apply(prim("map"), f, e); }
    public static <A> TypedTerm<A> mapList(TypedTerm<?> f, TypedTerm<?> xs) { return Phantoms.apply(prim("mapList"), f, xs); }
    public static <A> TypedTerm<A> mapOptional(TypedTerm<?> f, TypedTerm<?> x) { return Phantoms.apply(prim("mapOptional"), f, x); }
    public static <A> TypedTerm<A> mapSet(TypedTerm<?> f, TypedTerm<?> s) { return Phantoms.apply(prim("mapSet"), f, s); }
    public static <A> TypedTerm<A> partitionEithers(TypedTerm<?> eithers) { return Phantoms.apply(prim("partitionEithers"), eithers); }
    public static <A> TypedTerm<A> rights(TypedTerm<?> eithers) { return Phantoms.apply(prim("rights"), eithers); }
}
