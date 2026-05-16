package hydra.dsl.meta;

import hydra.core.Binding;
import hydra.core.Field;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.Namespace;
import hydra.packaging.TermDefinition;
import hydra.phantoms.TBinding;
import hydra.phantoms.TTerm;
import hydra.util.Maybe;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Term-level phantom-typed DSL. Java analogue of
 * {@code hydra.dsl.meta.phantoms} in Python and {@code Hydra.Dsl.Meta.Phantoms}
 * in Haskell.
 *
 * <p>All functions take {@link TTerm} arguments and return a {@link TTerm}, so that
 * the Java type system tracks the phantom types of intermediate terms. The
 * actual underlying values are built via {@link hydra.dsl.Terms}.</p>
 */
public final class Phantoms {
    private Phantoms() {}

    // ---- Term lifting ----

    /**
     * Lift a raw {@link Term} into a phantom-typed {@link TTerm}.
     * Mostly used internally; prefer the typed constructors below.
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public static <A> TTerm<A> tterm(Term t) {
        return new TTerm(t);
    }

    /** Extract the underlying {@link Term} from a {@link TTerm}. */
    public static <A> Term unTTerm(TTerm<A> t) {
        return t.value;
    }

    // ---- Variables and lambdas ----

    /** {@code var "x"} — reference a variable, either local (single-segment
     *  name like "x") or global (fully-qualified like "hydra.foo.bar").
     *  The underlying Term construction is identical in both cases. */
    public static <A> TTerm<A> var(String name) {
        return tterm(Terms.var(name));
    }

    /** {@code var name} — Name overload, useful when the name comes from a
     *  generated TYPE_/field constant or {@link hydra.dsl.meta.Defs.Def#name()}. */
    public static <A> TTerm<A> var(Name name) {
        return tterm(Terms.variable(name));
    }

    /** {@code lambda "x" body} — single-parameter lambda. */
    public static <A> TTerm<A> lambda(String param, TTerm<?> body) {
        return tterm(Terms.lambda(param, body.value));
    }

    /** Two-parameter lambda chain: {@code lambda "x" (lambda "y" body)}. */
    public static <A> TTerm<A> lambda(String p1, String p2, TTerm<?> body) {
        return lambda(p1, lambda(p2, body));
    }

    /** Three-parameter lambda chain. */
    public static <A> TTerm<A> lambda(String p1, String p2, String p3, TTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, body)));
    }

    /** Four-parameter lambda chain. */
    public static <A> TTerm<A> lambda(String p1, String p2, String p3, String p4, TTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, lambda(p4, body))));
    }

    /** Five-parameter lambda chain. */
    public static <A> TTerm<A> lambda(String p1, String p2, String p3, String p4, String p5, TTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, lambda(p4, lambda(p5, body)))));
    }

    /** Six-parameter lambda chain. */
    public static <A> TTerm<A> lambda(String p1, String p2, String p3, String p4, String p5, String p6, TTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, lambda(p4, lambda(p5, lambda(p6, body))))));
    }

    /** Seven-parameter lambda chain. */
    public static <A> TTerm<A> lambda(String p1, String p2, String p3, String p4, String p5, String p6, String p7, TTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, lambda(p4, lambda(p5, lambda(p6, lambda(p7, body)))))));
    }

    /** {@code lambda ["x","y", ...] body} — list-of-params variant for chains of arbitrary length. */
    public static <A> TTerm<A> lambda(List<String> params, TTerm<?> body) {
        TTerm<A> result = (TTerm<A>) body;
        for (int i = params.size() - 1; i >= 0; i--) {
            result = lambda(params.get(i), result);
        }
        return result;
    }

    /** {@code lambdaTyped "x" :: paramType body} — typed single-parameter lambda. */
    public static <A> TTerm<A> lamTyped(String param, Type paramType, TTerm<?> body) {
        return tterm(Terms.lambdaTyped(param, paramType, body.value));
    }

    /** {@code constant body} — \_ -> body (unused parameter "_"). */
    public static <A> TTerm<A> constant(TTerm<?> body) {
        return tterm(Terms.lambda("_", body.value));
    }

    // ---- Application ----

    /** {@code f @@ x} — apply a function to an argument. */
    public static <A> TTerm<A> apply(TTerm<?> fun, TTerm<?> arg) {
        return tterm(Terms.apply(fun.value, arg.value));
    }

    /** Two-argument application. */
    public static <A> TTerm<A> apply(TTerm<?> fun, TTerm<?> a, TTerm<?> b) {
        return tterm(Terms.apply(Terms.apply(fun.value, a.value), b.value));
    }

    /** Three-argument application. */
    public static <A> TTerm<A> apply(TTerm<?> fun, TTerm<?> a, TTerm<?> b, TTerm<?> c) {
        return tterm(Terms.apply(Terms.apply(Terms.apply(fun.value, a.value), b.value), c.value));
    }

    /** Four-argument application. */
    public static <A> TTerm<A> apply(TTerm<?> fun, TTerm<?> a, TTerm<?> b, TTerm<?> c, TTerm<?> d) {
        Term r = Terms.apply(fun.value, a.value);
        r = Terms.apply(r, b.value);
        r = Terms.apply(r, c.value);
        r = Terms.apply(r, d.value);
        return tterm(r);
    }

    /** Variadic application: {@code apply(f, a1, a2, ..., an)}.
     *
     * <p>Used when the call has more than 4 arguments — for 1..4 args the
     * fixed-arity overloads above resolve first (Java prefers them over
     * varargs).
     */
    public static <A> TTerm<A> apply(TTerm<?> fun, TTerm<?> a, TTerm<?> b, TTerm<?> c, TTerm<?> d, TTerm<?>... rest) {
        Term r = Terms.apply(fun.value, a.value);
        r = Terms.apply(r, b.value);
        r = Terms.apply(r, c.value);
        r = Terms.apply(r, d.value);
        for (TTerm<?> x : rest) {
            r = Terms.apply(r, x.value);
        }
        return tterm(r);
    }

    // ---- Let bindings ----

    /** {@code let name value body} — single-binding let. */
    public static <A> TTerm<A> let(String name, TTerm<?> value, TTerm<?> body) {
        return tterm(Terms.lets(
            Collections.singletonList(Terms.field(name, value.value)),
            body.value));
    }

    /** {@code let [field1, field2, ...] body} — multi-binding let (varargs).
     *  The last argument is the body; preceding arguments are field bindings. */
    public static <A> TTerm<A> let(Field b1, Field b2, TTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2), body.value));
    }
    public static <A> TTerm<A> let(Field b1, Field b2, Field b3, TTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3), body.value));
    }
    public static <A> TTerm<A> let(Field b1, Field b2, Field b3, Field b4, TTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3, b4), body.value));
    }
    public static <A> TTerm<A> let(Field b1, Field b2, Field b3, Field b4, Field b5, TTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3, b4, b5), body.value));
    }
    public static <A> TTerm<A> let(Field b1, Field b2, Field b3, Field b4, Field b5, Field b6, TTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3, b4, b5, b6), body.value));
    }
    public static <A> TTerm<A> let(Field b1, Field b2, Field b3, Field b4, Field b5, Field b6, Field b7, TTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3, b4, b5, b6, b7), body.value));
    }

    /** {@code let [bindings...] body} — list-of-bindings variant for chains of arbitrary length. */
    public static <A> TTerm<A> let(List<Field> bindings, TTerm<?> body) {
        return tterm(Terms.lets(bindings, body.value));
    }

    /**
     * Build a typed let: {@code let [name :: ts >: value] body}.
     * Produces a {@code Term.Let} (not a simple let-binding via Field), used
     * when each binding needs an explicit type scheme attached (e.g. pre-typed
     * Coder.java auto-ports that skip type inference).
     */
    public static <A> TTerm<A> letTyped(String name, TTerm<?> value, hydra.core.TypeScheme scheme, TTerm<?> body) {
        hydra.core.Binding b = new hydra.core.Binding(new Name(name), value.value, Maybe.just(scheme));
        return tterm(new hydra.core.Term.Let(new hydra.core.Let(Collections.singletonList(b), body.value)));
    }


    /** Build a let-binding field: {@code name >>: value}. */
    public static Field field(String name, TTerm<?> value) {
        return Terms.field(name, value.value);
    }

    /** Build a record/case field with an explicit Name. */
    public static Field field(Name name, TTerm<?> value) {
        return new Field(name, value.value);
    }

    // ---- Literals ----

    public static TTerm<String> string(String s) { return tterm(Terms.string(s)); }
    public static TTerm<Integer> int32(int i) { return tterm(Terms.int32(i)); }
    public static TTerm<Long> int64(long l) { return tterm(Terms.int64(l)); }
    public static TTerm<BigInteger> bigint(BigInteger b) { return tterm(Terms.bigint(b)); }
    public static TTerm<BigInteger> bigint(long b) { return tterm(Terms.bigint(BigInteger.valueOf(b))); }
    public static TTerm<Boolean> bool(boolean b) { return tterm(Terms.boolean_(b)); }
    public static TTerm<Double> float64(double d) { return tterm(Terms.float64(d)); }
    public static TTerm<Float> float32(float f) { return tterm(Terms.float32(f)); }

    // ---- Unit, maybe, either, pair, lists ----

    /** {@code unit}. */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public static <A> TTerm<A> unit() {
        return tterm(Terms.unit());
    }

    /** {@code just x}. */
    public static <A> TTerm<Maybe<A>> just(TTerm<A> x) {
        return tterm(Terms.just(x.value));
    }

    /** {@code nothing}. */
    public static <A> TTerm<Maybe<A>> nothing() {
        return tterm(Terms.nothing());
    }

    /** {@code right x} — Either.right. */
    public static <A> TTerm<A> right(TTerm<?> x) {
        return tterm(Terms.right(x.value));
    }

    /** {@code left x} — Either.left. */
    public static <A> TTerm<A> left(TTerm<?> x) {
        return tterm(Terms.left(x.value));
    }

    /** {@code pair a b}. */
    public static <A, B> TTerm<hydra.util.Pair<A, B>> pair(TTerm<A> a, TTerm<B> b) {
        return tterm(Terms.pair(a.value, b.value));
    }

    /** {@code list [elems...]}. */
    @SafeVarargs
    public static <A> TTerm<List<A>> list(TTerm<? extends A>... elems) {
        Term[] vs = new Term[elems.length];
        for (int i = 0; i < elems.length; i++) vs[i] = elems[i].value;
        return tterm(Terms.list(vs));
    }

    /** {@code list [elems...]} from a List. */
    public static <A> TTerm<List<A>> list(List<? extends TTerm<? extends A>> elems) {
        Term[] vs = new Term[elems.size()];
        for (int i = 0; i < elems.size(); i++) vs[i] = elems.get(i).value;
        return tterm(Terms.list(vs));
    }

    // ---- Records, injections, projections, wraps ----

    /** {@code record TypeName [field1, field2, ...]}. */
    public static <A> TTerm<A> record(String typeName, Field... fields) {
        return tterm(Terms.record(typeName, fields));
    }

    /** {@code record TypeName [field1, field2, ...]} — Name overload. */
    public static <A> TTerm<A> record(Name typeName, Field... fields) {
        return tterm(Terms.record(typeName.value, fields));
    }

    /** {@code record TypeName fields}. */
    public static <A> TTerm<A> record(String typeName, List<Field> fields) {
        return tterm(Terms.record(typeName, fields.toArray(new Field[0])));
    }

    /** {@code record TypeName fields} — Name overload. */
    public static <A> TTerm<A> record(Name typeName, List<Field> fields) {
        return tterm(Terms.record(typeName.value, fields.toArray(new Field[0])));
    }

    /** {@code inject TypeName fieldName value}. */
    public static <A> TTerm<A> inject(String typeName, String fieldName, TTerm<?> value) {
        return tterm(Terms.inject(typeName, fieldName, value.value));
    }

    /** {@code inject TypeName fieldName value} — Name overload. */
    public static <A> TTerm<A> inject(Name typeName, Name fieldName, TTerm<?> value) {
        return tterm(Terms.inject(typeName.value, fieldName.value, value.value));
    }

    /** {@code injectUnit TypeName fieldName} — variant case carrying unit. */
    public static <A> TTerm<A> injectUnit(String typeName, String fieldName) {
        return tterm(Terms.inject(typeName, fieldName, Terms.unit()));
    }

    /** {@code injectUnit TypeName fieldName} — Name overload. */
    public static <A> TTerm<A> injectUnit(Name typeName, Name fieldName) {
        return tterm(Terms.inject(typeName.value, fieldName.value, Terms.unit()));
    }

    /** {@code project TypeName fieldName} — field projection function. */
    public static <A> TTerm<A> project(String typeName, String fieldName) {
        return tterm(Terms.project(typeName, fieldName));
    }

    /** {@code project TypeName fieldName} — Name overload. */
    public static <A> TTerm<A> project(Name typeName, Name fieldName) {
        return tterm(Terms.project(typeName.value, fieldName.value));
    }

    /** {@code project TypeName fieldName @@ var varName} — combined projection + application.
     *  Collapses the common {@code apply(project(T, F), var("x"))} pattern. */
    public static <A> TTerm<A> proj(String typeName, String fieldName, String varName) {
        return apply(project(typeName, fieldName), var(varName));
    }

    /** {@code project TypeName fieldName @@ var varName} — Name overload. */
    public static <A> TTerm<A> proj(Name typeName, Name fieldName, String varName) {
        return apply(project(typeName, fieldName), var(varName));
    }

    /** {@code project TypeName fieldName @@ term} — Name overload, term argument. */
    public static <A> TTerm<A> proj(Name typeName, Name fieldName, TTerm<?> term) {
        return apply(project(typeName, fieldName), term);
    }

    /** {@code project TypeName fieldName @@ term} — String overload, term argument. */
    public static <A> TTerm<A> proj(String typeName, String fieldName, TTerm<?> term) {
        return apply(project(typeName, fieldName), term);
    }

    /** {@code wrap TypeName body}. */
    public static <A> TTerm<A> wrap(String typeName, TTerm<?> body) {
        return tterm(Terms.wrap(typeName, body.value));
    }

    /** {@code wrap TypeName body} — Name overload. */
    public static <A> TTerm<A> wrap(Name typeName, TTerm<?> body) {
        return tterm(Terms.wrap(typeName.value, body.value));
    }

    /** {@code unwrap TypeName} — wrapper-elimination function. */
    public static <A> TTerm<A> unwrap(String typeName) {
        return tterm(Terms.unwrap(typeName));
    }

    /** {@code unwrap TypeName} — Name overload. */
    public static <A> TTerm<A> unwrap(Name typeName) {
        return tterm(Terms.unwrap(typeName.value));
    }

    // ---- Pattern matching ----

    /** {@code cases TypeName x Nothing [branches]} — pattern match on union. */
    public static <A> TTerm<A> cases(String typeName, TTerm<?> arg, Field... branches) {
        Term match = Terms.match(typeName, Maybe.<Term>nothing(), branches);
        return tterm(Terms.apply(match, arg.value));
    }

    /** {@code cases TypeName x Nothing [branches]} — Name overload. */
    public static <A> TTerm<A> cases(Name typeName, TTerm<?> arg, Field... branches) {
        Term match = Terms.match(typeName.value, Maybe.<Term>nothing(), branches);
        return tterm(Terms.apply(match, arg.value));
    }

    /** {@code cases TypeName x (Just default) [branches]} — pattern match with default. */
    public static <A> TTerm<A> casesWithDefault(String typeName, TTerm<?> arg,
                                                 TTerm<?> defaultBranch, Field... branches) {
        Term match = Terms.match(typeName, Maybe.<Term>just(defaultBranch.value), branches);
        return tterm(Terms.apply(match, arg.value));
    }

    /** {@code cases TypeName x (Just default) [branches]} — Name overload. */
    public static <A> TTerm<A> casesWithDefault(Name typeName, TTerm<?> arg,
                                                 TTerm<?> defaultBranch, Field... branches) {
        Term match = Terms.match(typeName.value, Maybe.<Term>just(defaultBranch.value), branches);
        return tterm(Terms.apply(match, arg.value));
    }

    /** {@code match TypeName Nothing [branches]} — match function (not yet applied). */
    public static <A> TTerm<A> match(String typeName, Field... branches) {
        return tterm(Terms.match(typeName, Maybe.<Term>nothing(), branches));
    }

    /** {@code match TypeName Nothing [branches]} — Name overload. */
    public static <A> TTerm<A> match(Name typeName, Field... branches) {
        return tterm(Terms.match(typeName.value, Maybe.<Term>nothing(), branches));
    }

    /** {@code match TypeName (Just default) [branches]}. */
    public static <A> TTerm<A> matchWithDefault(String typeName, TTerm<?> defaultBranch,
                                                 Field... branches) {
        return tterm(Terms.match(typeName, Maybe.<Term>just(defaultBranch.value), branches));
    }

    /** {@code match TypeName (Just default) [branches]} — Name overload. */
    public static <A> TTerm<A> matchWithDefault(Name typeName, TTerm<?> defaultBranch,
                                                 Field... branches) {
        return tterm(Terms.match(typeName.value, Maybe.<Term>just(defaultBranch.value), branches));
    }

    // ---- Type application / type lambda ----

    /** {@code term :: forall ts. body} — apply a type argument. */
    public static <A> TTerm<A> tyapp(TTerm<?> term, Type type) {
        return tterm(Terms.tyapp(term.value, type));
    }

    /** {@code tyapps term [t1, t2, ...]} — apply multiple type arguments. */
    public static <A> TTerm<A> tyapps(TTerm<?> term, Type... types) {
        Term r = term.value;
        for (Type t : types) {
            r = Terms.tyapp(r, t);
        }
        return tterm(r);
    }

    /** {@code typeLambda [v1, v2, ...] body}. */
    public static <A> TTerm<A> typeLambda(List<String> vars, TTerm<?> body) {
        List<Name> names = new java.util.ArrayList<>();
        for (String v : vars) names.add(new Name(v));
        return tterm(Terms.typeLambda(names, body.value));
    }

    /** Single-variable type lambda. */
    public static <A> TTerm<A> typeLambda(String var, TTerm<?> body) {
        return typeLambda(Collections.singletonList(var), body);
    }

    // ---- Annotations / documentation ----

    /** Attach a description annotation to a term. */
    public static <A> TTerm<A> doc(String description, TTerm<A> term) {
        return tterm(Terms.annot(description, term.value));
    }

    // ---- Definitions ----

    /** Build a TBinding for a definition in a module. */
    public static <A> TBinding<A> definitionInModule(Module mod, String localName, TTerm<A> term) {
        Name fqName = new Name(mod.namespace.value + "." + localName);
        return new TBinding<A>(fqName, term);
    }

    /** Convert a TBinding to a Definition (without explicit type scheme). */
    public static <A> Definition toDefinition(TBinding<A> tb) {
        return new Definition.Term(new TermDefinition(
            tb.name,
            tb.term.value,
            Maybe.<TypeScheme>nothing()));
    }

    /** Build a Definition directly: namespace + localName + term, no TBinding. */
    public static <A> Definition def(Namespace ns, String localName, TTerm<A> term) {
        Name fqName = new Name(ns.value + "." + localName);
        return new Definition.Term(new TermDefinition(
            fqName,
            term.value,
            Maybe.<TypeScheme>nothing()));
    }

    /** Build a Definition with a pre-computed TypeScheme (used for hydra.java.coder). */
    public static <A> Definition defTyped(Namespace ns, String localName, TTerm<A> term,
                                            TypeScheme ts) {
        Name fqName = new Name(ns.value + "." + localName);
        return new Definition.Term(new TermDefinition(
            fqName,
            term.value,
            Maybe.<TypeScheme>just(ts)));
    }
}
