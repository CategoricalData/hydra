package hydra.dsl.meta.lib;

import hydra.core.Name;
import hydra.dsl.meta.Expr;
import hydra.phantoms.TTerm;

import static hydra.dsl.meta.Phantoms.*;

/**
 * Phantom-typed term DSL for the hydra.lib.logic library.
 *
 * <p>Mirrors {@code Hydra.Dsl.Meta.Lib.Logic} (Haskell) and
 * {@code hydra.dsl.meta.lib.logic} (Python).
 *
 * <p>Usage: {@code import static hydra.dsl.meta.lib.Logic.*;}
 */
public interface Logic {

    Name _logic_and = new Name("hydra.lib.logic.and");
    Name _logic_ifElse = new Name("hydra.lib.logic.ifElse");
    Name _logic_not = new Name("hydra.lib.logic.not");
    Name _logic_or = new Name("hydra.lib.logic.or");

    static <R> Expr<R> and(TTerm<?> x, TTerm<?> y) {
        return primitive2(_logic_and, x, y);
    }

    static <R> Expr<R> ifElse(TTerm<?> cond, TTerm<?> then_, TTerm<?> else_) {
        return primitive3(_logic_ifElse, cond, then_, else_);
    }

    static <R> Expr<R> not(TTerm<?> x) {
        return primitive1(_logic_not, x);
    }

    static <R> Expr<R> or(TTerm<?> x, TTerm<?> y) {
        return primitive2(_logic_or, x, y);
    }
}
