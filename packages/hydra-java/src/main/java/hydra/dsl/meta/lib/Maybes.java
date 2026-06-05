package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/**
 * Phantom-typed term DSL for the {@code hydra.lib.maybes} library.
 *
 * <p>Java analogue of {@code hydra.dsl.meta.lib.maybes} in Python and
 * {@code Hydra.Dsl.Meta.Lib.Maybes} in Haskell.</p>
 *
 * <p>Each method here builds an {@code apply}-chain calling the corresponding
 * primitive function (e.g. {@code hydra.lib.maybes.fromMaybe}).</p>
 */
public final class Maybes {
    private Maybes() {}

    /** Reference to {@code hydra.lib.maybes.<name>}. */
    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.maybes." + name);
    }

    /** {@code Maybes.apply f x} — apply a function in Maybe context (applicative). */
    public static <A> TypedTerm<A> apply(TypedTerm<?> f, TypedTerm<?> x) {
        return Phantoms.apply(prim("apply"), f, x);
    }

    /** {@code Maybes.bind x f} — chain operations on optional values. */
    public static <A> TypedTerm<A> bind(TypedTerm<?> x, TypedTerm<?> f) {
        return Phantoms.apply(prim("bind"), x, f);
    }

    /** {@code Maybes.cases m default just_handler}. */
    public static <A> TypedTerm<A> cases(TypedTerm<?> m, TypedTerm<?> defaultBranch, TypedTerm<?> justBranch) {
        return Phantoms.apply(prim("cases"), m, defaultBranch, justBranch);
    }

    /** {@code Maybes.cat xs} — keep only Just values from a list. */
    public static <A> TypedTerm<A> cat(TypedTerm<?> xs) {
        return Phantoms.apply(prim("cat"), xs);
    }

    /** {@code Maybes.compose f g}. */
    public static <A> TypedTerm<A> compose(TypedTerm<?> f, TypedTerm<?> g) {
        return Phantoms.apply(prim("compose"), f, g);
    }

    /** {@code Maybes.fromJust x} — extract value (unsafe if Nothing). */
    public static <A> TypedTerm<A> fromJust(TypedTerm<?> x) {
        return Phantoms.apply(prim("fromJust"), x);
    }

    /** {@code Maybes.fromMaybe default x} — get the value or default. */
    public static <A> TypedTerm<A> fromMaybe(TypedTerm<?> defaultVal, TypedTerm<?> x) {
        return Phantoms.apply(prim("fromMaybe"), defaultVal, x);
    }

    /** {@code Maybes.isJust x}. */
    public static <A> TypedTerm<A> isJust(TypedTerm<?> x) {
        return Phantoms.apply(prim("isJust"), x);
    }

    /** {@code Maybes.isNothing x}. */
    public static <A> TypedTerm<A> isNothing(TypedTerm<?> x) {
        return Phantoms.apply(prim("isNothing"), x);
    }

    /** {@code Maybes.map f x}. */
    public static <A> TypedTerm<A> map(TypedTerm<?> f, TypedTerm<?> x) {
        return Phantoms.apply(prim("map"), f, x);
    }

    /** {@code Maybes.mapMaybe f xs}. */
    public static <A> TypedTerm<A> mapMaybe(TypedTerm<?> f, TypedTerm<?> xs) {
        return Phantoms.apply(prim("mapMaybe"), f, xs);
    }

    /** {@code Maybes.pure x} — wrap in Just. */
    public static <A> TypedTerm<A> pure(TypedTerm<?> x) {
        return Phantoms.apply(prim("pure"), x);
    }

    /** {@code Maybes.toList x} — Just x → [x], Nothing → []. */
    public static <A> TypedTerm<A> toList(TypedTerm<?> x) {
        return Phantoms.apply(prim("toList"), x);
    }
}
