package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/**
 * Phantom-typed term DSL for the {@code hydra.lib.lists} library.
 *
 * <p>Java analogue of {@code hydra.dsl.meta.lib.lists} in Python and
 * {@code Hydra.Dsl.Meta.Lib.Lists} in Haskell.</p>
 */
public final class Lists {
    private Lists() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.lists." + name);
    }

    public static <A> TypedTerm<A> apply(TypedTerm<?> fs, TypedTerm<?> values) {
        return Phantoms.apply(prim("apply"), fs, values);
    }
    public static <A> TypedTerm<A> bind(TypedTerm<?> values, TypedTerm<?> f) {
        return Phantoms.apply(prim("bind"), values, f);
    }
    public static <A> TypedTerm<A> concat(TypedTerm<?> values) {
        return Phantoms.apply(prim("concat"), values);
    }
    public static <A> TypedTerm<A> concat2(TypedTerm<?> values1, TypedTerm<?> values2) {
        return Phantoms.apply(prim("concat2"), values1, values2);
    }
    public static <A> TypedTerm<A> cons(TypedTerm<?> value, TypedTerm<?> values) {
        return Phantoms.apply(prim("cons"), value, values);
    }
    public static <A> TypedTerm<A> drop(TypedTerm<?> n, TypedTerm<?> values) {
        return Phantoms.apply(prim("drop"), n, values);
    }
    public static <A> TypedTerm<A> dropWhile(TypedTerm<?> predicate, TypedTerm<?> values) {
        return Phantoms.apply(prim("dropWhile"), predicate, values);
    }
    public static <A> TypedTerm<A> elem(TypedTerm<?> value, TypedTerm<?> values) {
        return Phantoms.apply(prim("elem"), value, values);
    }
    public static <A> TypedTerm<A> filter(TypedTerm<?> f, TypedTerm<?> values) {
        return Phantoms.apply(prim("filter"), f, values);
    }
    public static <A> TypedTerm<A> find(TypedTerm<?> predicate, TypedTerm<?> values) {
        return Phantoms.apply(prim("find"), predicate, values);
    }
    public static <A> TypedTerm<A> foldl(TypedTerm<?> f, TypedTerm<?> initial, TypedTerm<?> values) {
        return Phantoms.apply(prim("foldl"), f, initial, values);
    }
    public static <A> TypedTerm<A> foldr(TypedTerm<?> f, TypedTerm<?> initial, TypedTerm<?> values) {
        return Phantoms.apply(prim("foldr"), f, initial, values);
    }
    public static <A> TypedTerm<A> group(TypedTerm<?> values) {
        return Phantoms.apply(prim("group"), values);
    }
    public static <A> TypedTerm<A> intercalate(TypedTerm<?> separator, TypedTerm<?> values) {
        return Phantoms.apply(prim("intercalate"), separator, values);
    }
    public static <A> TypedTerm<A> intersperse(TypedTerm<?> separator, TypedTerm<?> values) {
        return Phantoms.apply(prim("intersperse"), separator, values);
    }
    public static <A> TypedTerm<A> length(TypedTerm<?> values) {
        return Phantoms.apply(prim("length"), values);
    }
    public static <A> TypedTerm<A> map(TypedTerm<?> f, TypedTerm<?> values) {
        return Phantoms.apply(prim("map"), f, values);
    }
    public static <A> TypedTerm<A> maybeAt(TypedTerm<?> i, TypedTerm<?> values) {
        return Phantoms.apply(prim("maybeAt"), i, values);
    }
    public static <A> TypedTerm<A> maybeHead(TypedTerm<?> values) {
        return Phantoms.apply(prim("maybeHead"), values);
    }
    public static <A> TypedTerm<A> maybeInit(TypedTerm<?> values) {
        return Phantoms.apply(prim("maybeInit"), values);
    }
    public static <A> TypedTerm<A> maybeLast(TypedTerm<?> values) {
        return Phantoms.apply(prim("maybeLast"), values);
    }
    public static <A> TypedTerm<A> maybeTail(TypedTerm<?> values) {
        return Phantoms.apply(prim("maybeTail"), values);
    }
    public static <A> TypedTerm<A> nub(TypedTerm<?> values) {
        return Phantoms.apply(prim("nub"), values);
    }
    /** Note: method name 'null' is reserved in Java; using {@code null_}. */
    public static <A> TypedTerm<A> null_(TypedTerm<?> values) {
        return Phantoms.apply(prim("null"), values);
    }
    public static <A> TypedTerm<A> partition(TypedTerm<?> predicate, TypedTerm<?> values) {
        return Phantoms.apply(prim("partition"), predicate, values);
    }
    public static <A> TypedTerm<A> pure(TypedTerm<?> value) {
        return Phantoms.apply(prim("pure"), value);
    }
    public static <A> TypedTerm<A> replicate(TypedTerm<?> n, TypedTerm<?> value) {
        return Phantoms.apply(prim("replicate"), n, value);
    }
    public static <A> TypedTerm<A> reverse(TypedTerm<?> values) {
        return Phantoms.apply(prim("reverse"), values);
    }
    public static <A> TypedTerm<A> singleton(TypedTerm<?> value) {
        return Phantoms.apply(prim("singleton"), value);
    }
    public static <A> TypedTerm<A> sort(TypedTerm<?> values) {
        return Phantoms.apply(prim("sort"), values);
    }
    public static <A> TypedTerm<A> sortOn(TypedTerm<?> key, TypedTerm<?> values) {
        return Phantoms.apply(prim("sortOn"), key, values);
    }
    public static <A> TypedTerm<A> span(TypedTerm<?> predicate, TypedTerm<?> values) {
        return Phantoms.apply(prim("span"), predicate, values);
    }
    public static <A> TypedTerm<A> take(TypedTerm<?> n, TypedTerm<?> values) {
        return Phantoms.apply(prim("take"), n, values);
    }
    public static <A> TypedTerm<A> transpose(TypedTerm<?> values) {
        return Phantoms.apply(prim("transpose"), values);
    }
    public static <A> TypedTerm<A> uncons(TypedTerm<?> values) {
        return Phantoms.apply(prim("uncons"), values);
    }
    public static <A> TypedTerm<A> zip(TypedTerm<?> values1, TypedTerm<?> values2) {
        return Phantoms.apply(prim("zip"), values1, values2);
    }
    public static <A> TypedTerm<A> zipWith(TypedTerm<?> f, TypedTerm<?> values1, TypedTerm<?> values2) {
        return Phantoms.apply(prim("zipWith"), f, values1, values2);
    }
}
