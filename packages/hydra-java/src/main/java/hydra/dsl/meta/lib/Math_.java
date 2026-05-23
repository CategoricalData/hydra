package hydra.dsl.meta.lib;
import hydra.dsl.meta.Phantoms;
import hydra.phantoms.TTerm;

/**
 * Phantom-typed term DSL for the {@code hydra.lib.math} library.
 *
 * <p>Named {@code Math_} (trailing underscore) to avoid clashing with
 * {@link java.lang.Math}. Import as {@code Math_} or as {@code Math}
 * with an explicit alias.</p>
 */
public final class Math_ {
    private Math_() {}

    private static <A> TTerm<A> prim(String name) {
        return Phantoms.var("hydra.lib.math." + name);
    }

    public static <A> TTerm<A> abs_(TTerm<?> x) { return Phantoms.apply(prim("abs"), x); }
    public static <A> TTerm<A> acos(TTerm<?> x) { return Phantoms.apply(prim("acos"), x); }
    public static <A> TTerm<A> acosh(TTerm<?> x) { return Phantoms.apply(prim("acosh"), x); }
    public static <A> TTerm<A> add(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("add"), x, y); }
    public static <A> TTerm<A> addFloat64(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("addFloat64"), x, y); }
    public static <A> TTerm<A> asin(TTerm<?> x) { return Phantoms.apply(prim("asin"), x); }
    public static <A> TTerm<A> asinh(TTerm<?> x) { return Phantoms.apply(prim("asinh"), x); }
    public static <A> TTerm<A> atan(TTerm<?> x) { return Phantoms.apply(prim("atan"), x); }
    public static <A> TTerm<A> atan2(TTerm<?> y, TTerm<?> x) { return Phantoms.apply(prim("atan2"), y, x); }
    public static <A> TTerm<A> atanh(TTerm<?> x) { return Phantoms.apply(prim("atanh"), x); }
    public static <A> TTerm<A> ceiling(TTerm<?> x) { return Phantoms.apply(prim("ceiling"), x); }
    public static <A> TTerm<A> cos(TTerm<?> x) { return Phantoms.apply(prim("cos"), x); }
    public static <A> TTerm<A> cosh(TTerm<?> x) { return Phantoms.apply(prim("cosh"), x); }
    public static <A> TTerm<A> e() { return prim("e"); }
    public static <A> TTerm<A> even_(TTerm<?> x) { return Phantoms.apply(prim("even"), x); }
    public static <A> TTerm<A> exp(TTerm<?> x) { return Phantoms.apply(prim("exp"), x); }
    public static <A> TTerm<A> floor(TTerm<?> x) { return Phantoms.apply(prim("floor"), x); }
    public static <A> TTerm<A> log(TTerm<?> x) { return Phantoms.apply(prim("log"), x); }
    public static <A> TTerm<A> logBase(TTerm<?> base, TTerm<?> x) { return Phantoms.apply(prim("logBase"), base, x); }
    public static <A> TTerm<A> max_(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("max"), x, y); }
    public static <A> TTerm<A> maybeDiv(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("maybeDiv"), x, y); }
    public static <A> TTerm<A> maybeMod(TTerm<?> a, TTerm<?> b) { return Phantoms.apply(prim("maybeMod"), a, b); }
    public static <A> TTerm<A> maybePred(TTerm<?> x) { return Phantoms.apply(prim("maybePred"), x); }
    public static <A> TTerm<A> maybeRem(TTerm<?> a, TTerm<?> b) { return Phantoms.apply(prim("maybeRem"), a, b); }
    public static <A> TTerm<A> maybeSucc(TTerm<?> x) { return Phantoms.apply(prim("maybeSucc"), x); }
    public static <A> TTerm<A> min_(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("min"), x, y); }
    public static <A> TTerm<A> mul(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("mul"), x, y); }
    public static <A> TTerm<A> mulFloat64(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("mulFloat64"), x, y); }
    public static <A> TTerm<A> negate(TTerm<?> x) { return Phantoms.apply(prim("negate"), x); }
    public static <A> TTerm<A> negateFloat64(TTerm<?> x) { return Phantoms.apply(prim("negateFloat64"), x); }
    public static <A> TTerm<A> odd_(TTerm<?> x) { return Phantoms.apply(prim("odd"), x); }
    public static <A> TTerm<A> pi() { return prim("pi"); }
    public static <A> TTerm<A> pow_(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("pow"), x, y); }
    public static <A> TTerm<A> range_(TTerm<?> start, TTerm<?> end) { return Phantoms.apply(prim("range"), start, end); }
    public static <A> TTerm<A> round_(TTerm<?> x) { return Phantoms.apply(prim("round"), x); }
    public static <A> TTerm<A> roundFloat32(TTerm<?> digits, TTerm<?> x) { return Phantoms.apply(prim("roundFloat32"), digits, x); }
    public static <A> TTerm<A> roundFloat64(TTerm<?> digits, TTerm<?> x) { return Phantoms.apply(prim("roundFloat64"), digits, x); }
    public static <A> TTerm<A> signum(TTerm<?> x) { return Phantoms.apply(prim("signum"), x); }
    public static <A> TTerm<A> sin(TTerm<?> x) { return Phantoms.apply(prim("sin"), x); }
    public static <A> TTerm<A> sinh(TTerm<?> x) { return Phantoms.apply(prim("sinh"), x); }
    public static <A> TTerm<A> sqrt(TTerm<?> x) { return Phantoms.apply(prim("sqrt"), x); }
    public static <A> TTerm<A> sub(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("sub"), x, y); }
    public static <A> TTerm<A> subFloat64(TTerm<?> x, TTerm<?> y) { return Phantoms.apply(prim("subFloat64"), x, y); }
    public static <A> TTerm<A> tan(TTerm<?> x) { return Phantoms.apply(prim("tan"), x); }
    public static <A> TTerm<A> tanh(TTerm<?> x) { return Phantoms.apply(prim("tanh"), x); }
    public static <A> TTerm<A> truncate(TTerm<?> x) { return Phantoms.apply(prim("truncate"), x); }
}
