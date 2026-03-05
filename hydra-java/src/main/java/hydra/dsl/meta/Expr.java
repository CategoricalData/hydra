package hydra.dsl.meta;

import hydra.core.Application;
import hydra.core.Name;
import hydra.core.Term;
import hydra.phantoms.TTerm;

/**
 * An extensible phantom-typed term, extending the generated {@link TTerm}.
 *
 * <p>Adds {@link #of(TTerm)} for left-associative function application,
 * mirroring Haskell's {@code @@} operator and Python's {@code @} in
 * {@code Hydra.Dsl.Meta.Phantoms}.
 *
 * <p>All handwritten DSL methods ({@link Phantoms}, {@link Core}, etc.) return
 * {@code Expr} so that {@code .of()} is available for chaining:
 * <pre>
 *   // Before:  apply(apply(apply(f, a), b), c)
 *   // After:   f.of(a).of(b).of(c)
 * </pre>
 *
 * <p>Since {@code Expr<A> extends TTerm<A>}, it is assignment-compatible
 * everywhere a {@code TTerm<A>} is expected, including {@link hydra.phantoms.TBinding}.
 */
public class Expr<A> extends TTerm<A> {

    public Expr(Term value) {
        super(value);
    }

    /**
     * Apply this term to an argument (left-associative function application).
     *
     * <p>Mirrors Haskell's {@code @@} operator and Python's {@code @}:
     * <pre>
     *   f @@ a @@ b   ≡   f.of(a).of(b)
     * </pre>
     */
    @SuppressWarnings("unchecked")
    public <R> Expr<R> of(TTerm<?> arg) {
        return new Expr<>(new Term.Application(new Application(this.value, arg.value)));
    }

    /**
     * Apply this term to a variable by name.
     * Shorthand for {@code .of(var(name))}.
     *
     * <pre>
     *   f.of("x")   ≡   f.of(var("x"))
     * </pre>
     */
    @SuppressWarnings("unchecked")
    public <R> Expr<R> of(String varName) {
        Term varTerm = new Term.Variable(new Name(varName));
        return new Expr<>(new Term.Application(new Application(this.value, varTerm)));
    }
}
