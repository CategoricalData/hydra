package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.chars} library. */
public final class Chars {
    private Chars() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.chars." + name);
    }

    public static <A> TTerm<A> isAlphaNum(TTerm<?> c) { return Phantoms.apply(prim("isAlphaNum"), c); }
    public static <A> TTerm<A> isLower(TTerm<?> c) { return Phantoms.apply(prim("isLower"), c); }
    public static <A> TTerm<A> isSpace(TTerm<?> c) { return Phantoms.apply(prim("isSpace"), c); }
    public static <A> TTerm<A> isUpper(TTerm<?> c) { return Phantoms.apply(prim("isUpper"), c); }
    public static <A> TTerm<A> toLower(TTerm<?> c) { return Phantoms.apply(prim("toLower"), c); }
    public static <A> TTerm<A> toUpper(TTerm<?> c) { return Phantoms.apply(prim("toUpper"), c); }
}
