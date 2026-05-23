package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.eithers} library. */
public final class Eithers {
    private Eithers() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.eithers." + name);
    }

    public static <A> TTerm<A> bimap(TTerm<?> f, TTerm<?> g, TTerm<?> e) { return Phantoms.apply(prim("bimap"), f, g, e); }
    public static <A> TTerm<A> bind(TTerm<?> e, TTerm<?> f) { return Phantoms.apply(prim("bind"), e, f); }
    public static <A> TTerm<A> either_(TTerm<?> f, TTerm<?> g, TTerm<?> e) { return Phantoms.apply(prim("either"), f, g, e); }
    public static <A> TTerm<A> foldl(TTerm<?> f, TTerm<?> init, TTerm<?> xs) { return Phantoms.apply(prim("foldl"), f, init, xs); }
    public static <A> TTerm<A> fromLeft(TTerm<?> defaultVal, TTerm<?> e) { return Phantoms.apply(prim("fromLeft"), defaultVal, e); }
    public static <A> TTerm<A> fromRight(TTerm<?> defaultVal, TTerm<?> e) { return Phantoms.apply(prim("fromRight"), defaultVal, e); }
    public static <A> TTerm<A> isLeft(TTerm<?> e) { return Phantoms.apply(prim("isLeft"), e); }
    public static <A> TTerm<A> isRight(TTerm<?> e) { return Phantoms.apply(prim("isRight"), e); }
    public static <A> TTerm<A> lefts(TTerm<?> eithers) { return Phantoms.apply(prim("lefts"), eithers); }
    public static <A> TTerm<A> map(TTerm<?> f, TTerm<?> e) { return Phantoms.apply(prim("map"), f, e); }
    public static <A> TTerm<A> mapList(TTerm<?> f, TTerm<?> xs) { return Phantoms.apply(prim("mapList"), f, xs); }
    public static <A> TTerm<A> mapMaybe(TTerm<?> f, TTerm<?> x) { return Phantoms.apply(prim("mapMaybe"), f, x); }
    public static <A> TTerm<A> mapSet(TTerm<?> f, TTerm<?> s) { return Phantoms.apply(prim("mapSet"), f, s); }
    public static <A> TTerm<A> partitionEithers(TTerm<?> eithers) { return Phantoms.apply(prim("partitionEithers"), eithers); }
    public static <A> TTerm<A> rights(TTerm<?> eithers) { return Phantoms.apply(prim("rights"), eithers); }
}
