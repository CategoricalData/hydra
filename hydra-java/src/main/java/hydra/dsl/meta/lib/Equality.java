package hydra.dsl.meta.lib;

import hydra.core.Name;
import hydra.dsl.meta.Expr;
import hydra.phantoms.TTerm;

import static hydra.dsl.meta.Phantoms.*;

/**
 * Phantom-typed term DSL for the hydra.lib.equality library.
 *
 * <p>Mirrors {@code Hydra.Dsl.Meta.Lib.Equality} (Haskell) and
 * {@code hydra.dsl.meta.lib.equality} (Python).
 *
 * <p>Usage: {@code import static hydra.dsl.meta.lib.Equality.*;}
 */
public interface Equality {

    Name _equality_compare = new Name("hydra.lib.equality.compare");
    Name _equality_equal = new Name("hydra.lib.equality.equal");
    Name _equality_gt = new Name("hydra.lib.equality.gt");
    Name _equality_gte = new Name("hydra.lib.equality.gte");
    Name _equality_identity = new Name("hydra.lib.equality.identity");
    Name _equality_lt = new Name("hydra.lib.equality.lt");
    Name _equality_lte = new Name("hydra.lib.equality.lte");
    Name _equality_max = new Name("hydra.lib.equality.max");
    Name _equality_min = new Name("hydra.lib.equality.min");

    static <R> Expr<R> compare(TTerm<?> x, TTerm<?> y) {
        return primitive2(_equality_compare, x, y);
    }

    static <R> Expr<R> equal(TTerm<?> a, TTerm<?> b) {
        return primitive2(_equality_equal, a, b);
    }

    static <R> Expr<R> gt(TTerm<?> a, TTerm<?> b) {
        return primitive2(_equality_gt, a, b);
    }

    static <R> Expr<R> gte(TTerm<?> a, TTerm<?> b) {
        return primitive2(_equality_gte, a, b);
    }

    static <R> Expr<R> identity(TTerm<?> a) {
        return primitive1(_equality_identity, a);
    }

    static <R> Expr<R> lt(TTerm<?> a, TTerm<?> b) {
        return primitive2(_equality_lt, a, b);
    }

    static <R> Expr<R> lte(TTerm<?> a, TTerm<?> b) {
        return primitive2(_equality_lte, a, b);
    }

    static <R> Expr<R> max(TTerm<?> a, TTerm<?> b) {
        return primitive2(_equality_max, a, b);
    }

    static <R> Expr<R> min(TTerm<?> a, TTerm<?> b) {
        return primitive2(_equality_min, a, b);
    }
}
