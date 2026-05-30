package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.typed.TypedTerm;

/**
 * Phantom-typed term DSL for the {@code hydra.lib.math} library.
 *
 * <p>Named {@code Math_} (trailing underscore) to avoid clashing with
 * {@link java.lang.Math}. Import as {@code Math_} or as {@code Math}
 * with an explicit alias.</p>
 */
public final class Math_ {
    private Math_() {}

    private static <A> TypedTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.math." + name);
    }

    public static <A> TypedTerm<A> abs_(TypedTerm<?> x) { return Phantoms.apply(prim("abs"), x); }
    public static <A> TypedTerm<A> acos(TypedTerm<?> x) { return Phantoms.apply(prim("acos"), x); }
    public static <A> TypedTerm<A> acosh(TypedTerm<?> x) { return Phantoms.apply(prim("acosh"), x); }
    public static <A> TypedTerm<A> add(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("add"), x, y); }
    public static <A> TypedTerm<A> addFloat64(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("addFloat64"), x, y); }
    public static <A> TypedTerm<A> asin(TypedTerm<?> x) { return Phantoms.apply(prim("asin"), x); }
    public static <A> TypedTerm<A> asinh(TypedTerm<?> x) { return Phantoms.apply(prim("asinh"), x); }
    public static <A> TypedTerm<A> atan(TypedTerm<?> x) { return Phantoms.apply(prim("atan"), x); }
    public static <A> TypedTerm<A> atan2(TypedTerm<?> y, TypedTerm<?> x) { return Phantoms.apply(prim("atan2"), y, x); }
    public static <A> TypedTerm<A> atanh(TypedTerm<?> x) { return Phantoms.apply(prim("atanh"), x); }
    public static <A> TypedTerm<A> ceiling(TypedTerm<?> x) { return Phantoms.apply(prim("ceiling"), x); }
    public static <A> TypedTerm<A> cos(TypedTerm<?> x) { return Phantoms.apply(prim("cos"), x); }
    public static <A> TypedTerm<A> cosh(TypedTerm<?> x) { return Phantoms.apply(prim("cosh"), x); }
    public static <A> TypedTerm<A> e() { return prim("e"); }
    public static <A> TypedTerm<A> even_(TypedTerm<?> x) { return Phantoms.apply(prim("even"), x); }
    public static <A> TypedTerm<A> exp(TypedTerm<?> x) { return Phantoms.apply(prim("exp"), x); }
    public static <A> TypedTerm<A> floor(TypedTerm<?> x) { return Phantoms.apply(prim("floor"), x); }
    public static <A> TypedTerm<A> log(TypedTerm<?> x) { return Phantoms.apply(prim("log"), x); }
    public static <A> TypedTerm<A> logBase(TypedTerm<?> base, TypedTerm<?> x) { return Phantoms.apply(prim("logBase"), base, x); }
    public static <A> TypedTerm<A> max_(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("max"), x, y); }
    public static <A> TypedTerm<A> maybeDiv(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("maybeDiv"), x, y); }
    public static <A> TypedTerm<A> maybeMod(TypedTerm<?> a, TypedTerm<?> b) { return Phantoms.apply(prim("maybeMod"), a, b); }
    public static <A> TypedTerm<A> maybePred(TypedTerm<?> x) { return Phantoms.apply(prim("maybePred"), x); }
    public static <A> TypedTerm<A> maybeRem(TypedTerm<?> a, TypedTerm<?> b) { return Phantoms.apply(prim("maybeRem"), a, b); }
    public static <A> TypedTerm<A> maybeSucc(TypedTerm<?> x) { return Phantoms.apply(prim("maybeSucc"), x); }
    public static <A> TypedTerm<A> min_(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("min"), x, y); }
    public static <A> TypedTerm<A> mul(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("mul"), x, y); }
    public static <A> TypedTerm<A> mulFloat64(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("mulFloat64"), x, y); }
    public static <A> TypedTerm<A> negate(TypedTerm<?> x) { return Phantoms.apply(prim("negate"), x); }
    public static <A> TypedTerm<A> negateFloat64(TypedTerm<?> x) { return Phantoms.apply(prim("negateFloat64"), x); }
    public static <A> TypedTerm<A> odd_(TypedTerm<?> x) { return Phantoms.apply(prim("odd"), x); }
    public static <A> TypedTerm<A> pi() { return prim("pi"); }
    public static <A> TypedTerm<A> pow_(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("pow"), x, y); }
    public static <A> TypedTerm<A> range_(TypedTerm<?> start, TypedTerm<?> end) { return Phantoms.apply(prim("range"), start, end); }
    public static <A> TypedTerm<A> round_(TypedTerm<?> x) { return Phantoms.apply(prim("round"), x); }
    public static <A> TypedTerm<A> roundFloat32(TypedTerm<?> digits, TypedTerm<?> x) { return Phantoms.apply(prim("roundFloat32"), digits, x); }
    public static <A> TypedTerm<A> roundFloat64(TypedTerm<?> digits, TypedTerm<?> x) { return Phantoms.apply(prim("roundFloat64"), digits, x); }
    public static <A> TypedTerm<A> signum(TypedTerm<?> x) { return Phantoms.apply(prim("signum"), x); }
    public static <A> TypedTerm<A> sin(TypedTerm<?> x) { return Phantoms.apply(prim("sin"), x); }
    public static <A> TypedTerm<A> sinh(TypedTerm<?> x) { return Phantoms.apply(prim("sinh"), x); }
    public static <A> TypedTerm<A> sqrt(TypedTerm<?> x) { return Phantoms.apply(prim("sqrt"), x); }
    public static <A> TypedTerm<A> sub(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("sub"), x, y); }
    public static <A> TypedTerm<A> subFloat64(TypedTerm<?> x, TypedTerm<?> y) { return Phantoms.apply(prim("subFloat64"), x, y); }
    public static <A> TypedTerm<A> tan(TypedTerm<?> x) { return Phantoms.apply(prim("tan"), x); }
    public static <A> TypedTerm<A> tanh(TypedTerm<?> x) { return Phantoms.apply(prim("tanh"), x); }
    public static <A> TypedTerm<A> truncate(TypedTerm<?> x) { return Phantoms.apply(prim("truncate"), x); }
}
