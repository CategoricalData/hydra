package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.maps} library. */
public final class Maps {
    private Maps() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.maps." + name);
    }

    public static <A> TypedTerm<A> alter(TypedTerm<?> f, TypedTerm<?> key, TypedTerm<?> mapping) { return Phantoms.apply(prim("alter"), f, key, mapping); }
    public static <A> TypedTerm<A> bimap(TypedTerm<?> f, TypedTerm<?> g, TypedTerm<?> mapping) { return Phantoms.apply(prim("bimap"), f, g, mapping); }
    public static <A> TypedTerm<A> delete(TypedTerm<?> key, TypedTerm<?> mapping) { return Phantoms.apply(prim("delete"), key, mapping); }
    public static <A> TypedTerm<A> elems(TypedTerm<?> mapping) { return Phantoms.apply(prim("elems"), mapping); }
    public static <A> TypedTerm<A> empty() { return prim("empty"); }
    public static <A> TypedTerm<A> filter(TypedTerm<?> predicate, TypedTerm<?> mapping) { return Phantoms.apply(prim("filter"), predicate, mapping); }
    public static <A> TypedTerm<A> filterWithKey(TypedTerm<?> predicate, TypedTerm<?> mapping) { return Phantoms.apply(prim("filterWithKey"), predicate, mapping); }
    public static <A> TypedTerm<A> findWithDefault(TypedTerm<?> defaultVal, TypedTerm<?> key, TypedTerm<?> mapping) { return Phantoms.apply(prim("findWithDefault"), defaultVal, key, mapping); }
    public static <A> TypedTerm<A> fromList(TypedTerm<?> pairs) { return Phantoms.apply(prim("fromList"), pairs); }
    public static <A> TypedTerm<A> insert(TypedTerm<?> key, TypedTerm<?> value, TypedTerm<?> mapping) { return Phantoms.apply(prim("insert"), key, value, mapping); }
    public static <A> TypedTerm<A> keys(TypedTerm<?> mapping) { return Phantoms.apply(prim("keys"), mapping); }
    public static <A> TypedTerm<A> lookup(TypedTerm<?> key, TypedTerm<?> mapping) { return Phantoms.apply(prim("lookup"), key, mapping); }
    public static <A> TypedTerm<A> map(TypedTerm<?> f, TypedTerm<?> mapping) { return Phantoms.apply(prim("map"), f, mapping); }
    public static <A> TypedTerm<A> mapKeys(TypedTerm<?> f, TypedTerm<?> mapping) { return Phantoms.apply(prim("mapKeys"), f, mapping); }
    public static <A> TypedTerm<A> member(TypedTerm<?> key, TypedTerm<?> mapping) { return Phantoms.apply(prim("member"), key, mapping); }
    public static <A> TypedTerm<A> null_(TypedTerm<?> mapping) { return Phantoms.apply(prim("null"), mapping); }
    public static <A> TypedTerm<A> singleton(TypedTerm<?> key, TypedTerm<?> value) { return Phantoms.apply(prim("singleton"), key, value); }
    public static <A> TypedTerm<A> size(TypedTerm<?> mapping) { return Phantoms.apply(prim("size"), mapping); }
    public static <A> TypedTerm<A> toList(TypedTerm<?> mapping) { return Phantoms.apply(prim("toList"), mapping); }
    public static <A> TypedTerm<A> union(TypedTerm<?> map1, TypedTerm<?> map2) { return Phantoms.apply(prim("union"), map1, map2); }
}
