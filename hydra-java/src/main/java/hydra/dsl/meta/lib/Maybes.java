package hydra.dsl.meta.lib;

import hydra.core.Name;
import hydra.dsl.meta.Expr;
import hydra.phantoms.TTerm;

import static hydra.dsl.meta.Phantoms.*;

/**
 * Phantom-typed term DSL for the hydra.lib.maybes library.
 *
 * <p>Mirrors {@code Hydra.Dsl.Meta.Lib.Maybes} (Haskell) and
 * {@code hydra.dsl.meta.lib.maybes} (Python).
 *
 * <p>Usage: {@code import static hydra.dsl.meta.lib.Maybes.*;}
 */
public interface Maybes {

    Name _maybes_apply = new Name("hydra.lib.maybes.apply");
    Name _maybes_bind = new Name("hydra.lib.maybes.bind");
    Name _maybes_cases = new Name("hydra.lib.maybes.cases");
    Name _maybes_cat = new Name("hydra.lib.maybes.cat");
    Name _maybes_compose = new Name("hydra.lib.maybes.compose");
    Name _maybes_fromJust = new Name("hydra.lib.maybes.fromJust");
    Name _maybes_fromMaybe = new Name("hydra.lib.maybes.fromMaybe");
    Name _maybes_isJust = new Name("hydra.lib.maybes.isJust");
    Name _maybes_isNothing = new Name("hydra.lib.maybes.isNothing");
    Name _maybes_map = new Name("hydra.lib.maybes.map");
    Name _maybes_mapMaybe = new Name("hydra.lib.maybes.mapMaybe");
    Name _maybes_maybe = new Name("hydra.lib.maybes.maybe");
    Name _maybes_pure = new Name("hydra.lib.maybes.pure");

    static <R> Expr<R> apply(TTerm<?> f, TTerm<?> x) {
        return primitive2(_maybes_apply, f, x);
    }

    static <R> Expr<R> bind(TTerm<?> x, TTerm<?> f) {
        return primitive2(_maybes_bind, x, f);
    }

    static <R> Expr<R> cases(TTerm<?> m, TTerm<?> n, TTerm<?> j) {
        return primitive3(_maybes_cases, m, n, j);
    }

    static <R> Expr<R> cat(TTerm<?> xs) {
        return primitive1(_maybes_cat, xs);
    }

    static <R> Expr<R> compose(TTerm<?> f, TTerm<?> g) {
        return primitive2(_maybes_compose, f, g);
    }

    static <R> Expr<R> fromJust(TTerm<?> x) {
        return primitive1(_maybes_fromJust, x);
    }

    static <R> Expr<R> fromMaybe(TTerm<?> def, TTerm<?> x) {
        return primitive2(_maybes_fromMaybe, def, x);
    }

    static <R> Expr<R> isJust(TTerm<?> x) {
        return primitive1(_maybes_isJust, x);
    }

    static <R> Expr<R> isNothing(TTerm<?> x) {
        return primitive1(_maybes_isNothing, x);
    }

    static <R> Expr<R> map(TTerm<?> f, TTerm<?> x) {
        return primitive2(_maybes_map, f, x);
    }

    static <R> Expr<R> mapMaybe(TTerm<?> f, TTerm<?> xs) {
        return primitive2(_maybes_mapMaybe, f, xs);
    }

    static <R> Expr<R> maybe(TTerm<?> def, TTerm<?> f, TTerm<?> x) {
        return primitive3(_maybes_maybe, def, f, x);
    }

    static <R> Expr<R> pure_(TTerm<?> x) {
        return primitive1(_maybes_pure, x);
    }
}
