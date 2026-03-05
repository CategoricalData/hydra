package hydra.dsl.meta.lib;

import hydra.core.Name;
import hydra.dsl.meta.Expr;
import hydra.phantoms.TTerm;

import static hydra.dsl.meta.Phantoms.*;

/**
 * Phantom-typed term DSL for the hydra.lib.lists library.
 *
 * <p>Mirrors {@code Hydra.Dsl.Meta.Lib.Lists} (Haskell) and
 * {@code hydra.dsl.meta.lib.lists} (Python).
 *
 * <p>Usage: {@code import static hydra.dsl.meta.lib.Lists.*;}
 */
public interface Lists {

    Name _lists_apply = new Name("hydra.lib.lists.apply");
    Name _lists_at = new Name("hydra.lib.lists.at");
    Name _lists_bind = new Name("hydra.lib.lists.bind");
    Name _lists_concat = new Name("hydra.lib.lists.concat");
    Name _lists_concat2 = new Name("hydra.lib.lists.concat2");
    Name _lists_cons = new Name("hydra.lib.lists.cons");
    Name _lists_drop = new Name("hydra.lib.lists.drop");
    Name _lists_dropWhile = new Name("hydra.lib.lists.dropWhile");
    Name _lists_elem = new Name("hydra.lib.lists.elem");
    Name _lists_filter = new Name("hydra.lib.lists.filter");
    Name _lists_find = new Name("hydra.lib.lists.find");
    Name _lists_foldl = new Name("hydra.lib.lists.foldl");
    Name _lists_group = new Name("hydra.lib.lists.group");
    Name _lists_head = new Name("hydra.lib.lists.head");
    Name _lists_init = new Name("hydra.lib.lists.init");
    Name _lists_intercalate = new Name("hydra.lib.lists.intercalate");
    Name _lists_intersperse = new Name("hydra.lib.lists.intersperse");
    Name _lists_last = new Name("hydra.lib.lists.last");
    Name _lists_length = new Name("hydra.lib.lists.length");
    Name _lists_map = new Name("hydra.lib.lists.map");
    Name _lists_nub = new Name("hydra.lib.lists.nub");
    Name _lists_null = new Name("hydra.lib.lists.null");
    Name _lists_partition = new Name("hydra.lib.lists.partition");
    Name _lists_pure = new Name("hydra.lib.lists.pure");
    Name _lists_replicate = new Name("hydra.lib.lists.replicate");
    Name _lists_reverse = new Name("hydra.lib.lists.reverse");
    Name _lists_safeHead = new Name("hydra.lib.lists.safeHead");
    Name _lists_singleton = new Name("hydra.lib.lists.singleton");
    Name _lists_sort = new Name("hydra.lib.lists.sort");
    Name _lists_sortOn = new Name("hydra.lib.lists.sortOn");
    Name _lists_span = new Name("hydra.lib.lists.span");
    Name _lists_tail = new Name("hydra.lib.lists.tail");
    Name _lists_take = new Name("hydra.lib.lists.take");
    Name _lists_transpose = new Name("hydra.lib.lists.transpose");
    Name _lists_zip = new Name("hydra.lib.lists.zip");
    Name _lists_zipWith = new Name("hydra.lib.lists.zipWith");

    static <R> Expr<R> apply(TTerm<?> fs, TTerm<?> values) {
        return primitive2(_lists_apply, fs, values);
    }

    static <R> Expr<R> at(TTerm<?> i, TTerm<?> values) {
        return primitive2(_lists_at, i, values);
    }

    static <R> Expr<R> bind(TTerm<?> values, TTerm<?> f) {
        return primitive2(_lists_bind, values, f);
    }

    static <R> Expr<R> concat(TTerm<?> xss) {
        return primitive1(_lists_concat, xss);
    }

    static <R> Expr<R> concat2(TTerm<?> xs, TTerm<?> ys) {
        return primitive2(_lists_concat2, xs, ys);
    }

    static <R> Expr<R> cons(TTerm<?> x, TTerm<?> xs) {
        return primitive2(_lists_cons, x, xs);
    }

    static <R> Expr<R> drop(TTerm<?> n, TTerm<?> xs) {
        return primitive2(_lists_drop, n, xs);
    }

    static <R> Expr<R> dropWhile(TTerm<?> predicate, TTerm<?> xs) {
        return primitive2(_lists_dropWhile, predicate, xs);
    }

    static <R> Expr<R> elem(TTerm<?> x, TTerm<?> xs) {
        return primitive2(_lists_elem, x, xs);
    }

    static <R> Expr<R> filter(TTerm<?> predicate, TTerm<?> xs) {
        return primitive2(_lists_filter, predicate, xs);
    }

    static <R> Expr<R> find(TTerm<?> predicate, TTerm<?> xs) {
        return primitive2(_lists_find, predicate, xs);
    }

    static <R> Expr<R> foldl(TTerm<?> f, TTerm<?> z, TTerm<?> xs) {
        return primitive3(_lists_foldl, f, z, xs);
    }

    static <R> Expr<R> group(TTerm<?> xs) {
        return primitive1(_lists_group, xs);
    }

    static <R> Expr<R> head(TTerm<?> xs) {
        return primitive1(_lists_head, xs);
    }

    static <R> Expr<R> init(TTerm<?> xs) {
        return primitive1(_lists_init, xs);
    }

    static <R> Expr<R> intercalate(TTerm<?> sep, TTerm<?> xss) {
        return primitive2(_lists_intercalate, sep, xss);
    }

    static <R> Expr<R> intersperse(TTerm<?> sep, TTerm<?> xs) {
        return primitive2(_lists_intersperse, sep, xs);
    }

    static <R> Expr<R> last(TTerm<?> xs) {
        return primitive1(_lists_last, xs);
    }

    static <R> Expr<R> length(TTerm<?> xs) {
        return primitive1(_lists_length, xs);
    }

    static <R> Expr<R> map(TTerm<?> f, TTerm<?> xs) {
        return primitive2(_lists_map, f, xs);
    }

    static <R> Expr<R> nub(TTerm<?> xs) {
        return primitive1(_lists_nub, xs);
    }

    static <R> Expr<R> null_(TTerm<?> xs) {
        return primitive1(_lists_null, xs);
    }

    static <R> Expr<R> partition(TTerm<?> predicate, TTerm<?> xs) {
        return primitive2(_lists_partition, predicate, xs);
    }

    static <R> Expr<R> pure_(TTerm<?> x) {
        return primitive1(_lists_pure, x);
    }

    static <R> Expr<R> replicate(TTerm<?> n, TTerm<?> x) {
        return primitive2(_lists_replicate, n, x);
    }

    static <R> Expr<R> reverse(TTerm<?> xs) {
        return primitive1(_lists_reverse, xs);
    }

    static <R> Expr<R> safeHead(TTerm<?> xs) {
        return primitive1(_lists_safeHead, xs);
    }

    static <R> Expr<R> singleton(TTerm<?> x) {
        return primitive1(_lists_singleton, x);
    }

    static <R> Expr<R> sort(TTerm<?> xs) {
        return primitive1(_lists_sort, xs);
    }

    static <R> Expr<R> sortOn(TTerm<?> key, TTerm<?> xs) {
        return primitive2(_lists_sortOn, key, xs);
    }

    static <R> Expr<R> span(TTerm<?> predicate, TTerm<?> xs) {
        return primitive2(_lists_span, predicate, xs);
    }

    static <R> Expr<R> tail(TTerm<?> xs) {
        return primitive1(_lists_tail, xs);
    }

    static <R> Expr<R> take(TTerm<?> n, TTerm<?> xs) {
        return primitive2(_lists_take, n, xs);
    }

    static <R> Expr<R> transpose(TTerm<?> xss) {
        return primitive1(_lists_transpose, xss);
    }

    static <R> Expr<R> zip(TTerm<?> xs, TTerm<?> ys) {
        return primitive2(_lists_zip, xs, ys);
    }

    static <R> Expr<R> zipWith(TTerm<?> f, TTerm<?> xs, TTerm<?> ys) {
        return primitive3(_lists_zipWith, f, xs, ys);
    }
}
