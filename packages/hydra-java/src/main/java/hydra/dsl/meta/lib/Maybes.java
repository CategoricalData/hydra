package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

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
    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.maybes." + name);
    }

    /** {@code Maybes.apply f x} — apply a function in Maybe context (applicative). */
    public static <A> TTerm<A> apply(TTerm<?> f, TTerm<?> x) {
        return Phantoms.apply(prim("apply"), f, x);
    }

    /** {@code Maybes.bind x f} — chain operations on optional values. */
    public static <A> TTerm<A> bind(TTerm<?> x, TTerm<?> f) {
        return Phantoms.apply(prim("bind"), x, f);
    }

    /** {@code Maybes.cases m default just_handler}. */
    public static <A> TTerm<A> cases(TTerm<?> m, TTerm<?> defaultBranch, TTerm<?> justBranch) {
        return Phantoms.apply(prim("cases"), m, defaultBranch, justBranch);
    }

    /** {@code Maybes.cat xs} — keep only Just values from a list. */
    public static <A> TTerm<A> cat(TTerm<?> xs) {
        return Phantoms.apply(prim("cat"), xs);
    }

    /** {@code Maybes.compose f g}. */
    public static <A> TTerm<A> compose(TTerm<?> f, TTerm<?> g) {
        return Phantoms.apply(prim("compose"), f, g);
    }

    /** {@code Maybes.fromJust x} — extract value (unsafe if Nothing). */
    public static <A> TTerm<A> fromJust(TTerm<?> x) {
        return Phantoms.apply(prim("fromJust"), x);
    }

    /** {@code Maybes.fromMaybe default x} — get the value or default. */
    public static <A> TTerm<A> fromMaybe(TTerm<?> defaultVal, TTerm<?> x) {
        return Phantoms.apply(prim("fromMaybe"), defaultVal, x);
    }

    /** {@code Maybes.isJust x}. */
    public static <A> TTerm<A> isJust(TTerm<?> x) {
        return Phantoms.apply(prim("isJust"), x);
    }

    /** {@code Maybes.isNothing x}. */
    public static <A> TTerm<A> isNothing(TTerm<?> x) {
        return Phantoms.apply(prim("isNothing"), x);
    }

    /** {@code Maybes.map f x}. */
    public static <A> TTerm<A> map(TTerm<?> f, TTerm<?> x) {
        return Phantoms.apply(prim("map"), f, x);
    }

    /** {@code Maybes.mapMaybe f xs}. */
    public static <A> TTerm<A> mapMaybe(TTerm<?> f, TTerm<?> xs) {
        return Phantoms.apply(prim("mapMaybe"), f, xs);
    }

    /** {@code Maybes.maybe default f x} — handle Maybe with transformation. */
    public static <A> TTerm<A> maybe(TTerm<?> defaultVal, TTerm<?> f, TTerm<?> x) {
        return Phantoms.apply(prim("maybe"), defaultVal, f, x);
    }

    /** {@code Maybes.pure x} — wrap in Just. */
    public static <A> TTerm<A> pure(TTerm<?> x) {
        return Phantoms.apply(prim("pure"), x);
    }

    /** {@code Maybes.toList x} — Just x → [x], Nothing → []. */
    public static <A> TTerm<A> toList(TTerm<?> x) {
        return Phantoms.apply(prim("toList"), x);
    }
}
