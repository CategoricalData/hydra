package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/**
 * Phantom-typed term DSL for the {@code hydra.lib.optionals} library.
 *
 * <p>Java analogue of {@code hydra.dsl.meta.lib.optionals} in Python and
 * {@code Hydra.Dsl.Meta.Lib.Optionals} in Haskell.</p>
 *
 * <p>Each method here builds an {@code apply}-chain calling the corresponding
 * primitive function (e.g. {@code hydra.lib.optionals.fromOptional}).</p>
 */
public final class Optionals {
    private Optionals() {}

    /** Reference to {@code hydra.lib.optionals.<name>}. */
    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.optionals." + name);
    }

    /** {@code Optionals.apply f x} — apply a function in Maybe context (applicative). */
    public static <A> TypedTerm<A> apply(TypedTerm<?> f, TypedTerm<?> x) {
        return Phantoms.apply(prim("apply"), f, x);
    }

    /** {@code Optionals.bind x f} — chain operations on optional values. */
    public static <A> TypedTerm<A> bind(TypedTerm<?> x, TypedTerm<?> f) {
        return Phantoms.apply(prim("bind"), x, f);
    }

    /** {@code Optionals.cases m default just_handler}. */
    public static <A> TypedTerm<A> cases(TypedTerm<?> m, TypedTerm<?> defaultBranch, TypedTerm<?> justBranch) {
        return Phantoms.apply(prim("cases"), m, defaultBranch, justBranch);
    }

    /** {@code Optionals.cat xs} — keep only Just values from a list. */
    public static <A> TypedTerm<A> cat(TypedTerm<?> xs) {
        return Phantoms.apply(prim("cat"), xs);
    }

    /** {@code Optionals.compose f g}. */
    public static <A> TypedTerm<A> compose(TypedTerm<?> f, TypedTerm<?> g) {
        return Phantoms.apply(prim("compose"), f, g);
    }

    /** {@code Optionals.fromJust x} — extract value (unsafe if Nothing). */
    public static <A> TypedTerm<A> fromJust(TypedTerm<?> x) {
        return Phantoms.apply(prim("fromJust"), x);
    }

    /** {@code Optionals.fromOptional default x} — get the value or default. */
    public static <A> TypedTerm<A> fromOptional(TypedTerm<?> defaultVal, TypedTerm<?> x) {
        return Phantoms.apply(prim("fromOptional"), defaultVal, x);
    }

    /** {@code Optionals.isGiven x}. */
    public static <A> TypedTerm<A> isGiven(TypedTerm<?> x) {
        return Phantoms.apply(prim("isGiven"), x);
    }

    /** {@code Optionals.isNone x}. */
    public static <A> TypedTerm<A> isNone(TypedTerm<?> x) {
        return Phantoms.apply(prim("isNone"), x);
    }

    /** {@code Optionals.map f x}. */
    public static <A> TypedTerm<A> map(TypedTerm<?> f, TypedTerm<?> x) {
        return Phantoms.apply(prim("map"), f, x);
    }

    /** {@code Optionals.mapOptional f xs}. */
    public static <A> TypedTerm<A> mapOptional(TypedTerm<?> f, TypedTerm<?> xs) {
        return Phantoms.apply(prim("mapOptional"), f, xs);
    }

    /** {@code Optionals.pure x} — wrap in Just. */
    public static <A> TypedTerm<A> pure(TypedTerm<?> x) {
        return Phantoms.apply(prim("pure"), x);
    }

    /** {@code Optionals.toList x} — Just x → [x], Nothing → []. */
    public static <A> TypedTerm<A> toList(TypedTerm<?> x) {
        return Phantoms.apply(prim("toList"), x);
    }
}
