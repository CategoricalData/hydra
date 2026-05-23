package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.maps} library. */
public final class Maps {
    private Maps() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.maps." + name);
    }

    public static <A> TTerm<A> alter(TTerm<?> f, TTerm<?> key, TTerm<?> mapping) { return Phantoms.apply(prim("alter"), f, key, mapping); }
    public static <A> TTerm<A> bimap(TTerm<?> f, TTerm<?> g, TTerm<?> mapping) { return Phantoms.apply(prim("bimap"), f, g, mapping); }
    public static <A> TTerm<A> delete(TTerm<?> key, TTerm<?> mapping) { return Phantoms.apply(prim("delete"), key, mapping); }
    public static <A> TTerm<A> elems(TTerm<?> mapping) { return Phantoms.apply(prim("elems"), mapping); }
    public static <A> TTerm<A> empty() { return prim("empty"); }
    public static <A> TTerm<A> filter(TTerm<?> predicate, TTerm<?> mapping) { return Phantoms.apply(prim("filter"), predicate, mapping); }
    public static <A> TTerm<A> filterWithKey(TTerm<?> predicate, TTerm<?> mapping) { return Phantoms.apply(prim("filterWithKey"), predicate, mapping); }
    public static <A> TTerm<A> findWithDefault(TTerm<?> defaultVal, TTerm<?> key, TTerm<?> mapping) { return Phantoms.apply(prim("findWithDefault"), defaultVal, key, mapping); }
    public static <A> TTerm<A> fromList(TTerm<?> pairs) { return Phantoms.apply(prim("fromList"), pairs); }
    public static <A> TTerm<A> insert(TTerm<?> key, TTerm<?> value, TTerm<?> mapping) { return Phantoms.apply(prim("insert"), key, value, mapping); }
    public static <A> TTerm<A> keys(TTerm<?> mapping) { return Phantoms.apply(prim("keys"), mapping); }
    public static <A> TTerm<A> lookup(TTerm<?> key, TTerm<?> mapping) { return Phantoms.apply(prim("lookup"), key, mapping); }
    public static <A> TTerm<A> map(TTerm<?> f, TTerm<?> mapping) { return Phantoms.apply(prim("map"), f, mapping); }
    public static <A> TTerm<A> mapKeys(TTerm<?> f, TTerm<?> mapping) { return Phantoms.apply(prim("mapKeys"), f, mapping); }
    public static <A> TTerm<A> member(TTerm<?> key, TTerm<?> mapping) { return Phantoms.apply(prim("member"), key, mapping); }
    public static <A> TTerm<A> null_(TTerm<?> mapping) { return Phantoms.apply(prim("null"), mapping); }
    public static <A> TTerm<A> singleton(TTerm<?> key, TTerm<?> value) { return Phantoms.apply(prim("singleton"), key, value); }
    public static <A> TTerm<A> size(TTerm<?> mapping) { return Phantoms.apply(prim("size"), mapping); }
    public static <A> TTerm<A> toList(TTerm<?> mapping) { return Phantoms.apply(prim("toList"), mapping); }
    public static <A> TTerm<A> union(TTerm<?> map1, TTerm<?> map2) { return Phantoms.apply(prim("union"), map1, map2); }
}
