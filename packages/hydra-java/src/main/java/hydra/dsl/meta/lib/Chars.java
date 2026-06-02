package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.chars} library. */
public final class Chars {
    private Chars() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.chars." + name);
    }

    public static <A> TypedTerm<A> isAlphaNum(TypedTerm<?> c) { return Phantoms.apply(prim("isAlphaNum"), c); }
    public static <A> TypedTerm<A> isLower(TypedTerm<?> c) { return Phantoms.apply(prim("isLower"), c); }
    public static <A> TypedTerm<A> isSpace(TypedTerm<?> c) { return Phantoms.apply(prim("isSpace"), c); }
    public static <A> TypedTerm<A> isUpper(TypedTerm<?> c) { return Phantoms.apply(prim("isUpper"), c); }
    public static <A> TypedTerm<A> toLower(TypedTerm<?> c) { return Phantoms.apply(prim("toLower"), c); }
    public static <A> TypedTerm<A> toUpper(TypedTerm<?> c) { return Phantoms.apply(prim("toUpper"), c); }
}
