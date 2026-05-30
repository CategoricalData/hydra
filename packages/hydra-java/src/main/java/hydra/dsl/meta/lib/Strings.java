package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/** Phantom-typed term DSL for the {@code hydra.lib.strings} library. */
public final class Strings {
    private Strings() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.strings." + name);
    }

    public static <A> TypedTerm<A> cat(TypedTerm<?> strings) { return Phantoms.apply(prim("cat"), strings); }
    public static <A> TypedTerm<A> cat2(TypedTerm<?> s1, TypedTerm<?> s2) { return Phantoms.apply(prim("cat2"), s1, s2); }
    public static <A> TypedTerm<A> fromList(TypedTerm<?> chars) { return Phantoms.apply(prim("fromList"), chars); }
    public static <A> TypedTerm<A> intercalate(TypedTerm<?> separator, TypedTerm<?> strings) { return Phantoms.apply(prim("intercalate"), separator, strings); }
    public static <A> TypedTerm<A> length(TypedTerm<?> s) { return Phantoms.apply(prim("length"), s); }
    public static <A> TypedTerm<A> lines(TypedTerm<?> s) { return Phantoms.apply(prim("lines"), s); }
    public static <A> TypedTerm<A> maybeCharAt(TypedTerm<?> index, TypedTerm<?> s) { return Phantoms.apply(prim("maybeCharAt"), index, s); }
    public static <A> TypedTerm<A> null_(TypedTerm<?> s) { return Phantoms.apply(prim("null"), s); }
    public static <A> TypedTerm<A> splitOn(TypedTerm<?> separator, TypedTerm<?> s) { return Phantoms.apply(prim("splitOn"), separator, s); }
    public static <A> TypedTerm<A> toList(TypedTerm<?> s) { return Phantoms.apply(prim("toList"), s); }
    public static <A> TypedTerm<A> toLower(TypedTerm<?> s) { return Phantoms.apply(prim("toLower"), s); }
    public static <A> TypedTerm<A> toUpper(TypedTerm<?> s) { return Phantoms.apply(prim("toUpper"), s); }
    public static <A> TypedTerm<A> unlines(TypedTerm<?> strings) { return Phantoms.apply(prim("unlines"), strings); }
}
