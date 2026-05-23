package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/**
 * Phantom-typed term DSL for the {@code hydra.lib.lists} library.
 *
 * <p>Java analogue of {@code hydra.dsl.meta.lib.lists} in Python and
 * {@code Hydra.Dsl.Meta.Lib.Lists} in Haskell.</p>
 */
public final class Lists {
    private Lists() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.lists." + name);
    }

    public static <A> TTerm<A> apply(TTerm<?> fs, TTerm<?> values) {
        return Phantoms.apply(prim("apply"), fs, values);
    }
    public static <A> TTerm<A> bind(TTerm<?> values, TTerm<?> f) {
        return Phantoms.apply(prim("bind"), values, f);
    }
    public static <A> TTerm<A> concat(TTerm<?> values) {
        return Phantoms.apply(prim("concat"), values);
    }
    public static <A> TTerm<A> concat2(TTerm<?> values1, TTerm<?> values2) {
        return Phantoms.apply(prim("concat2"), values1, values2);
    }
    public static <A> TTerm<A> cons(TTerm<?> value, TTerm<?> values) {
        return Phantoms.apply(prim("cons"), value, values);
    }
    public static <A> TTerm<A> drop(TTerm<?> n, TTerm<?> values) {
        return Phantoms.apply(prim("drop"), n, values);
    }
    public static <A> TTerm<A> dropWhile(TTerm<?> predicate, TTerm<?> values) {
        return Phantoms.apply(prim("dropWhile"), predicate, values);
    }
    public static <A> TTerm<A> elem(TTerm<?> value, TTerm<?> values) {
        return Phantoms.apply(prim("elem"), value, values);
    }
    public static <A> TTerm<A> filter(TTerm<?> f, TTerm<?> values) {
        return Phantoms.apply(prim("filter"), f, values);
    }
    public static <A> TTerm<A> find(TTerm<?> predicate, TTerm<?> values) {
        return Phantoms.apply(prim("find"), predicate, values);
    }
    public static <A> TTerm<A> foldl(TTerm<?> f, TTerm<?> initial, TTerm<?> values) {
        return Phantoms.apply(prim("foldl"), f, initial, values);
    }
    public static <A> TTerm<A> foldr(TTerm<?> f, TTerm<?> initial, TTerm<?> values) {
        return Phantoms.apply(prim("foldr"), f, initial, values);
    }
    public static <A> TTerm<A> group(TTerm<?> values) {
        return Phantoms.apply(prim("group"), values);
    }
    public static <A> TTerm<A> intercalate(TTerm<?> separator, TTerm<?> values) {
        return Phantoms.apply(prim("intercalate"), separator, values);
    }
    public static <A> TTerm<A> intersperse(TTerm<?> separator, TTerm<?> values) {
        return Phantoms.apply(prim("intersperse"), separator, values);
    }
    public static <A> TTerm<A> length(TTerm<?> values) {
        return Phantoms.apply(prim("length"), values);
    }
    public static <A> TTerm<A> map(TTerm<?> f, TTerm<?> values) {
        return Phantoms.apply(prim("map"), f, values);
    }
    public static <A> TTerm<A> maybeAt(TTerm<?> i, TTerm<?> values) {
        return Phantoms.apply(prim("maybeAt"), i, values);
    }
    public static <A> TTerm<A> maybeHead(TTerm<?> values) {
        return Phantoms.apply(prim("maybeHead"), values);
    }
    public static <A> TTerm<A> maybeInit(TTerm<?> values) {
        return Phantoms.apply(prim("maybeInit"), values);
    }
    public static <A> TTerm<A> maybeLast(TTerm<?> values) {
        return Phantoms.apply(prim("maybeLast"), values);
    }
    public static <A> TTerm<A> maybeTail(TTerm<?> values) {
        return Phantoms.apply(prim("maybeTail"), values);
    }
    public static <A> TTerm<A> nub(TTerm<?> values) {
        return Phantoms.apply(prim("nub"), values);
    }
    /** Note: method name 'null' is reserved in Java; using {@code null_}. */
    public static <A> TTerm<A> null_(TTerm<?> values) {
        return Phantoms.apply(prim("null"), values);
    }
    public static <A> TTerm<A> partition(TTerm<?> predicate, TTerm<?> values) {
        return Phantoms.apply(prim("partition"), predicate, values);
    }
    public static <A> TTerm<A> pure(TTerm<?> value) {
        return Phantoms.apply(prim("pure"), value);
    }
    public static <A> TTerm<A> replicate(TTerm<?> n, TTerm<?> value) {
        return Phantoms.apply(prim("replicate"), n, value);
    }
    public static <A> TTerm<A> reverse(TTerm<?> values) {
        return Phantoms.apply(prim("reverse"), values);
    }
    public static <A> TTerm<A> singleton(TTerm<?> value) {
        return Phantoms.apply(prim("singleton"), value);
    }
    public static <A> TTerm<A> sort(TTerm<?> values) {
        return Phantoms.apply(prim("sort"), values);
    }
    public static <A> TTerm<A> sortOn(TTerm<?> key, TTerm<?> values) {
        return Phantoms.apply(prim("sortOn"), key, values);
    }
    public static <A> TTerm<A> span(TTerm<?> predicate, TTerm<?> values) {
        return Phantoms.apply(prim("span"), predicate, values);
    }
    public static <A> TTerm<A> take(TTerm<?> n, TTerm<?> values) {
        return Phantoms.apply(prim("take"), n, values);
    }
    public static <A> TTerm<A> transpose(TTerm<?> values) {
        return Phantoms.apply(prim("transpose"), values);
    }
    public static <A> TTerm<A> uncons(TTerm<?> values) {
        return Phantoms.apply(prim("uncons"), values);
    }
    public static <A> TTerm<A> zip(TTerm<?> values1, TTerm<?> values2) {
        return Phantoms.apply(prim("zip"), values1, values2);
    }
    public static <A> TTerm<A> zipWith(TTerm<?> f, TTerm<?> values1, TTerm<?> values2) {
        return Phantoms.apply(prim("zipWith"), f, values1, values2);
    }
}
