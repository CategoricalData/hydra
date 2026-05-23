package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.strings} library. */
public final class Strings {
    private Strings() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.strings." + name);
    }

    public static <A> TTerm<A> cat(TTerm<?> strings) { return Phantoms.apply(prim("cat"), strings); }
    public static <A> TTerm<A> cat2(TTerm<?> s1, TTerm<?> s2) { return Phantoms.apply(prim("cat2"), s1, s2); }
    public static <A> TTerm<A> fromList(TTerm<?> chars) { return Phantoms.apply(prim("fromList"), chars); }
    public static <A> TTerm<A> intercalate(TTerm<?> separator, TTerm<?> strings) { return Phantoms.apply(prim("intercalate"), separator, strings); }
    public static <A> TTerm<A> length(TTerm<?> s) { return Phantoms.apply(prim("length"), s); }
    public static <A> TTerm<A> lines(TTerm<?> s) { return Phantoms.apply(prim("lines"), s); }
    public static <A> TTerm<A> maybeCharAt(TTerm<?> index, TTerm<?> s) { return Phantoms.apply(prim("maybeCharAt"), index, s); }
    public static <A> TTerm<A> null_(TTerm<?> s) { return Phantoms.apply(prim("null"), s); }
    public static <A> TTerm<A> splitOn(TTerm<?> separator, TTerm<?> s) { return Phantoms.apply(prim("splitOn"), separator, s); }
    public static <A> TTerm<A> toList(TTerm<?> s) { return Phantoms.apply(prim("toList"), s); }
    public static <A> TTerm<A> toLower(TTerm<?> s) { return Phantoms.apply(prim("toLower"), s); }
    public static <A> TTerm<A> toUpper(TTerm<?> s) { return Phantoms.apply(prim("toUpper"), s); }
    public static <A> TTerm<A> unlines(TTerm<?> strings) { return Phantoms.apply(prim("unlines"), strings); }
}
