package hydra.dsl.meta.lib;

import hydra.core.Name;
import hydra.dsl.meta.Expr;
import hydra.phantoms.TTerm;

import static hydra.dsl.meta.Phantoms.*;

/**
 * Phantom-typed term DSL for the hydra.lib.sets library.
 *
 * <p>Mirrors {@code Hydra.Dsl.Meta.Lib.Sets} (Haskell) and
 * {@code hydra.dsl.meta.lib.sets} (Python).
 *
 * <p>Usage: {@code import static hydra.dsl.meta.lib.Sets.*;}
 */
public interface Sets {

    Name _sets_delete = new Name("hydra.lib.sets.delete");
    Name _sets_difference = new Name("hydra.lib.sets.difference");
    Name _sets_empty = new Name("hydra.lib.sets.empty");
    Name _sets_fromList = new Name("hydra.lib.sets.fromList");
    Name _sets_insert = new Name("hydra.lib.sets.insert");
    Name _sets_intersection = new Name("hydra.lib.sets.intersection");
    Name _sets_map = new Name("hydra.lib.sets.map");
    Name _sets_member = new Name("hydra.lib.sets.member");
    Name _sets_null = new Name("hydra.lib.sets.null");
    Name _sets_singleton = new Name("hydra.lib.sets.singleton");
    Name _sets_size = new Name("hydra.lib.sets.size");
    Name _sets_toList = new Name("hydra.lib.sets.toList");
    Name _sets_union = new Name("hydra.lib.sets.union");
    Name _sets_unions = new Name("hydra.lib.sets.unions");

    static <R> Expr<R> delete(TTerm<?> x, TTerm<?> s) {
        return primitive2(_sets_delete, x, s);
    }

    static <R> Expr<R> difference(TTerm<?> s1, TTerm<?> s2) {
        return primitive2(_sets_difference, s1, s2);
    }

    static <R> Expr<R> empty() {
        return primitive(_sets_empty);
    }

    static <R> Expr<R> fromList(TTerm<?> xs) {
        return primitive1(_sets_fromList, xs);
    }

    static <R> Expr<R> insert(TTerm<?> x, TTerm<?> s) {
        return primitive2(_sets_insert, x, s);
    }

    static <R> Expr<R> intersection(TTerm<?> s1, TTerm<?> s2) {
        return primitive2(_sets_intersection, s1, s2);
    }

    static <R> Expr<R> map(TTerm<?> f, TTerm<?> s) {
        return primitive2(_sets_map, f, s);
    }

    static <R> Expr<R> member(TTerm<?> x, TTerm<?> s) {
        return primitive2(_sets_member, x, s);
    }

    static <R> Expr<R> null_(TTerm<?> s) {
        return primitive1(_sets_null, s);
    }

    static <R> Expr<R> singleton(TTerm<?> x) {
        return primitive1(_sets_singleton, x);
    }

    static <R> Expr<R> size(TTerm<?> s) {
        return primitive1(_sets_size, s);
    }

    static <R> Expr<R> toList(TTerm<?> s) {
        return primitive1(_sets_toList, s);
    }

    static <R> Expr<R> union(TTerm<?> s1, TTerm<?> s2) {
        return primitive2(_sets_union, s1, s2);
    }

    static <R> Expr<R> unions(TTerm<?> sets) {
        return primitive1(_sets_unions, sets);
    }
}
