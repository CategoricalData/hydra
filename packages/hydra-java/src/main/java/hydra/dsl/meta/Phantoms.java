package hydra.dsl.meta;
import hydra.Scoping;
import hydra.core.Binding;
import hydra.core.Field;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.packaging.EntityMetadata;
import hydra.packaging.Definition;
import hydra.packaging.Module;
import hydra.packaging.ModuleName;
import hydra.packaging.TermDefinition;
import hydra.typed.TypedBinding;
import hydra.typed.TypedTerm;
import hydra.typing.TermSignature;
import hydra.util.Optional;

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
 * <p>All functions take {@link TypedTerm} arguments and return a {@link TypedTerm}, so that
 * the Java type system tracks the phantom types of intermediate terms. The
 * actual underlying values are built via {@link hydra.dsl.Terms}.</p>
 */
public final class Phantoms {
    private Phantoms() {}

    // ---- Term lifting ----

    /**
     * Lift a raw {@link Term} into a phantom-typed {@link TypedTerm}.
     * Mostly used internally; prefer the typed constructors below.
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public static <A> TypedTerm<A> tterm(Term t) {
        return new TypedTerm(t);
    }

    /** Extract the underlying {@link Term} from a {@link TypedTerm}. */
    public static <A> Term unTTerm(TypedTerm<A> t) {
        return t.value;
    }

    // ---- Variables and lambdas ----

    /** {@code var "x"} — reference a variable, either local (single-segment
     *  name like "x") or global (fully-qualified like "hydra.foo.bar").
     *  The underlying Term construction is identical in both cases. */
    public static <A> TypedTerm<A> var(String name) {
        return tterm(Terms.var(name));
    }

    /** {@code var name} — Name overload, useful when the name comes from a
     *  generated TYPE_/field constant or {@link hydra.dsl.meta.Defs.Def#name()}. */
    public static <A> TypedTerm<A> var(Name name) {
        return tterm(Terms.variable(name));
    }

    /** {@code lambda "x" body} — single-parameter lambda. */
    public static <A> TypedTerm<A> lambda(String param, TypedTerm<?> body) {
        return tterm(Terms.lambda(param, body.value));
    }

    /** Two-parameter lambda chain: {@code lambda "x" (lambda "y" body)}. */
    public static <A> TypedTerm<A> lambda(String p1, String p2, TypedTerm<?> body) {
        return lambda(p1, lambda(p2, body));
    }

    /** Three-parameter lambda chain. */
    public static <A> TypedTerm<A> lambda(String p1, String p2, String p3, TypedTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, body)));
    }

    /** Four-parameter lambda chain. */
    public static <A> TypedTerm<A> lambda(String p1, String p2, String p3, String p4, TypedTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, lambda(p4, body))));
    }

    /** Five-parameter lambda chain. */
    public static <A> TypedTerm<A> lambda(String p1, String p2, String p3, String p4, String p5, TypedTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, lambda(p4, lambda(p5, body)))));
    }

    /** Six-parameter lambda chain. */
    public static <A> TypedTerm<A> lambda(String p1, String p2, String p3, String p4, String p5, String p6, TypedTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, lambda(p4, lambda(p5, lambda(p6, body))))));
    }

    /** Seven-parameter lambda chain. */
    public static <A> TypedTerm<A> lambda(String p1, String p2, String p3, String p4, String p5, String p6, String p7, TypedTerm<?> body) {
        return lambda(p1, lambda(p2, lambda(p3, lambda(p4, lambda(p5, lambda(p6, lambda(p7, body)))))));
    }

    /** {@code lambda ["x","y", ...] body} — list-of-params variant for chains of arbitrary length. */
    public static <A> TypedTerm<A> lambda(List<String> params, TypedTerm<?> body) {
        TypedTerm<A> result = (TypedTerm<A>) body;
        for (int i = params.size() - 1; i >= 0; i--) {
            result = lambda(params.get(i), result);
        }
        return result;
    }

    /** {@code lambdaTyped "x" :: paramType body} — typed single-parameter lambda. */
    public static <A> TypedTerm<A> lamTyped(String param, Type paramType, TypedTerm<?> body) {
        return tterm(Terms.lambdaTyped(param, paramType, body.value));
    }

    /** {@code constant body} — \_ -> body (unused parameter "_"). */
    public static <A> TypedTerm<A> constant(TypedTerm<?> body) {
        return tterm(Terms.lambda("_", body.value));
    }

    // ---- Application ----

    /** {@code f @@ x} — apply a function to an argument. */
    public static <A> TypedTerm<A> apply(TypedTerm<?> fun, TypedTerm<?> arg) {
        return tterm(Terms.apply(fun.value, arg.value));
    }

    /** Two-argument application. */
    public static <A> TypedTerm<A> apply(TypedTerm<?> fun, TypedTerm<?> a, TypedTerm<?> b) {
        return tterm(Terms.apply(Terms.apply(fun.value, a.value), b.value));
    }

    /** Three-argument application. */
    public static <A> TypedTerm<A> apply(TypedTerm<?> fun, TypedTerm<?> a, TypedTerm<?> b, TypedTerm<?> c) {
        return tterm(Terms.apply(Terms.apply(Terms.apply(fun.value, a.value), b.value), c.value));
    }

    /** Four-argument application. */
    public static <A> TypedTerm<A> apply(TypedTerm<?> fun, TypedTerm<?> a, TypedTerm<?> b, TypedTerm<?> c, TypedTerm<?> d) {
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
    public static <A> TypedTerm<A> apply(TypedTerm<?> fun, TypedTerm<?> a, TypedTerm<?> b, TypedTerm<?> c, TypedTerm<?> d, TypedTerm<?>... rest) {
        Term r = Terms.apply(fun.value, a.value);
        r = Terms.apply(r, b.value);
        r = Terms.apply(r, c.value);
        r = Terms.apply(r, d.value);
        for (TypedTerm<?> x : rest) {
            r = Terms.apply(r, x.value);
        }
        return tterm(r);
    }

    // ---- Let bindings ----

    /** {@code let name value body} — single-binding let. */
    public static <A> TypedTerm<A> let(String name, TypedTerm<?> value, TypedTerm<?> body) {
        return tterm(Terms.lets(
            Collections.singletonList(Terms.field(name, value.value)),
            body.value));
    }

    /** {@code let [field1, field2, ...] body} — multi-binding let (varargs).
     *  The last argument is the body; preceding arguments are field bindings. */
    public static <A> TypedTerm<A> let(Field b1, Field b2, TypedTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2), body.value));
    }
    public static <A> TypedTerm<A> let(Field b1, Field b2, Field b3, TypedTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3), body.value));
    }
    public static <A> TypedTerm<A> let(Field b1, Field b2, Field b3, Field b4, TypedTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3, b4), body.value));
    }
    public static <A> TypedTerm<A> let(Field b1, Field b2, Field b3, Field b4, Field b5, TypedTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3, b4, b5), body.value));
    }
    public static <A> TypedTerm<A> let(Field b1, Field b2, Field b3, Field b4, Field b5, Field b6, TypedTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3, b4, b5, b6), body.value));
    }
    public static <A> TypedTerm<A> let(Field b1, Field b2, Field b3, Field b4, Field b5, Field b6, Field b7, TypedTerm<?> body) {
        return tterm(Terms.lets(Arrays.asList(b1, b2, b3, b4, b5, b6, b7), body.value));
    }

    /** {@code let [bindings...] body} — list-of-bindings variant for chains of arbitrary length. */
    public static <A> TypedTerm<A> let(List<Field> bindings, TypedTerm<?> body) {
        return tterm(Terms.lets(bindings, body.value));
    }

    /**
     * Build a typed let: {@code let [name :: ts >: value] body}.
     * Produces a {@code Term.Let} (not a simple let-binding via Field), used
     * when each binding needs an explicit type scheme attached (e.g. pre-typed
     * Coder.java auto-ports that skip type inference).
     */
    public static <A> TypedTerm<A> letTyped(String name, TypedTerm<?> value, hydra.core.TypeScheme scheme, TypedTerm<?> body) {
        hydra.core.Binding b = new hydra.core.Binding(new Name(name), value.value, Optional.given(scheme));
        return tterm(new hydra.core.Term.Let(new hydra.core.Let(Collections.singletonList(b), body.value)));
    }


    /** Build a let-binding field: {@code name >>: value}. */
    public static Field field(String name, TypedTerm<?> value) {
        return Terms.field(name, value.value);
    }

    /** Build a record/case field with an explicit Name. */
    public static Field field(Name name, TypedTerm<?> value) {
        return new Field(name, value.value);
    }

    // ---- Literals ----

    public static TypedTerm<String> string(String s) { return tterm(Terms.string(s)); }
    public static TypedTerm<Integer> int32(int i) { return tterm(Terms.int32(i)); }
    public static TypedTerm<Long> int64(long l) { return tterm(Terms.int64(l)); }
    public static TypedTerm<BigInteger> bigint(BigInteger b) { return tterm(Terms.bigint(b)); }
    public static TypedTerm<BigInteger> bigint(long b) { return tterm(Terms.bigint(BigInteger.valueOf(b))); }
    public static TypedTerm<Boolean> bool(boolean b) { return tterm(Terms.boolean_(b)); }
    public static TypedTerm<Double> float64(double d) { return tterm(Terms.float64(d)); }
    public static TypedTerm<Float> float32(float f) { return tterm(Terms.float32(f)); }

    // ---- Unit, maybe, either, pair, lists ----

    /** {@code unit}. */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public static <A> TypedTerm<A> unit() {
        return tterm(Terms.unit());
    }

    /** {@code just x}. */
    public static <A> TypedTerm<Optional<A>> just(TypedTerm<A> x) {
        return tterm(Terms.just(x.value));
    }

    /** {@code nothing}. */
    public static <A> TypedTerm<Optional<A>> nothing() {
        return tterm(Terms.nothing());
    }

    /** {@code right x} — Either.right. */
    public static <A> TypedTerm<A> right(TypedTerm<?> x) {
        return tterm(Terms.right(x.value));
    }

    /** {@code left x} — Either.left. */
    public static <A> TypedTerm<A> left(TypedTerm<?> x) {
        return tterm(Terms.left(x.value));
    }

    /** {@code pair a b}. */
    public static <A, B> TypedTerm<hydra.util.Pair<A, B>> pair(TypedTerm<A> a, TypedTerm<B> b) {
        return tterm(Terms.pair(a.value, b.value));
    }

    /** {@code list [elems...]}. */
    @SafeVarargs
    public static <A> TypedTerm<List<A>> list(TypedTerm<? extends A>... elems) {
        Term[] vs = new Term[elems.length];
        for (int i = 0; i < elems.length; i++) vs[i] = elems[i].value;
        return tterm(Terms.list(vs));
    }

    /** {@code list [elems...]} from a List. */
    public static <A> TypedTerm<List<A>> list(List<? extends TypedTerm<? extends A>> elems) {
        Term[] vs = new Term[elems.size()];
        for (int i = 0; i < elems.size(); i++) vs[i] = elems.get(i).value;
        return tterm(Terms.list(vs));
    }

    // ---- Records, injections, projections, wraps ----

    /** {@code record TypeName [field1, field2, ...]}. */
    public static <A> TypedTerm<A> record(String typeName, Field... fields) {
        return tterm(Terms.record(typeName, fields));
    }

    /** {@code record TypeName [field1, field2, ...]} — Name overload. */
    public static <A> TypedTerm<A> record(Name typeName, Field... fields) {
        return tterm(Terms.record(typeName.value, fields));
    }

    /** {@code record TypeName fields}. */
    public static <A> TypedTerm<A> record(String typeName, List<Field> fields) {
        return tterm(Terms.record(typeName, fields.toArray(new Field[0])));
    }

    /** {@code record TypeName fields} — Name overload. */
    public static <A> TypedTerm<A> record(Name typeName, List<Field> fields) {
        return tterm(Terms.record(typeName.value, fields.toArray(new Field[0])));
    }

    /** {@code inject TypeName fieldName value}. */
    public static <A> TypedTerm<A> inject(String typeName, String fieldName, TypedTerm<?> value) {
        return tterm(Terms.inject(typeName, fieldName, value.value));
    }

    /** {@code inject TypeName fieldName value} — Name overload. */
    public static <A> TypedTerm<A> inject(Name typeName, Name fieldName, TypedTerm<?> value) {
        return tterm(Terms.inject(typeName.value, fieldName.value, value.value));
    }

    /** {@code injectUnit TypeName fieldName} — variant case carrying unit. */
    public static <A> TypedTerm<A> injectUnit(String typeName, String fieldName) {
        return tterm(Terms.inject(typeName, fieldName, Terms.unit()));
    }

    /** {@code injectUnit TypeName fieldName} — Name overload. */
    public static <A> TypedTerm<A> injectUnit(Name typeName, Name fieldName) {
        return tterm(Terms.inject(typeName.value, fieldName.value, Terms.unit()));
    }

    /** {@code project TypeName fieldName} — field projection function. */
    public static <A> TypedTerm<A> project(String typeName, String fieldName) {
        return tterm(Terms.project(typeName, fieldName));
    }

    /** {@code project TypeName fieldName} — Name overload. */
    public static <A> TypedTerm<A> project(Name typeName, Name fieldName) {
        return tterm(Terms.project(typeName.value, fieldName.value));
    }

    /** {@code project TypeName fieldName @@ var varName} — combined projection + application.
     *  Collapses the common {@code apply(project(T, F), var("x"))} pattern. */
    public static <A> TypedTerm<A> proj(String typeName, String fieldName, String varName) {
        return apply(project(typeName, fieldName), var(varName));
    }

    /** {@code project TypeName fieldName @@ var varName} — Name overload. */
    public static <A> TypedTerm<A> proj(Name typeName, Name fieldName, String varName) {
        return apply(project(typeName, fieldName), var(varName));
    }

    /** {@code project TypeName fieldName @@ term} — Name overload, term argument. */
    public static <A> TypedTerm<A> proj(Name typeName, Name fieldName, TypedTerm<?> term) {
        return apply(project(typeName, fieldName), term);
    }

    /** {@code project TypeName fieldName @@ term} — String overload, term argument. */
    public static <A> TypedTerm<A> proj(String typeName, String fieldName, TypedTerm<?> term) {
        return apply(project(typeName, fieldName), term);
    }

    /** {@code wrap TypeName body}. */
    public static <A> TypedTerm<A> wrap(String typeName, TypedTerm<?> body) {
        return tterm(Terms.wrap(typeName, body.value));
    }

    /** {@code wrap TypeName body} — Name overload. */
    public static <A> TypedTerm<A> wrap(Name typeName, TypedTerm<?> body) {
        return tterm(Terms.wrap(typeName.value, body.value));
    }

    /** {@code unwrap TypeName} — wrapper-elimination function. */
    public static <A> TypedTerm<A> unwrap(String typeName) {
        return tterm(Terms.unwrap(typeName));
    }

    /** {@code unwrap TypeName} — Name overload. */
    public static <A> TypedTerm<A> unwrap(Name typeName) {
        return tterm(Terms.unwrap(typeName.value));
    }

    // ---- Pattern matching ----

    /** {@code cases TypeName x Nothing [branches]} — pattern match on union. */
    public static <A> TypedTerm<A> cases(String typeName, TypedTerm<?> arg, Field... branches) {
        Term match = Terms.match(typeName, Optional.<Term>none(), branches);
        return tterm(Terms.apply(match, arg.value));
    }

    /** {@code cases TypeName x Nothing [branches]} — Name overload. */
    public static <A> TypedTerm<A> cases(Name typeName, TypedTerm<?> arg, Field... branches) {
        Term match = Terms.match(typeName.value, Optional.<Term>none(), branches);
        return tterm(Terms.apply(match, arg.value));
    }

    /** {@code cases TypeName x (Just default) [branches]} — pattern match with default. */
    public static <A> TypedTerm<A> casesWithDefault(String typeName, TypedTerm<?> arg,
                                                 TypedTerm<?> defaultBranch, Field... branches) {
        Term match = Terms.match(typeName, Optional.<Term>given(defaultBranch.value), branches);
        return tterm(Terms.apply(match, arg.value));
    }

    /** {@code cases TypeName x (Just default) [branches]} — Name overload. */
    public static <A> TypedTerm<A> casesWithDefault(Name typeName, TypedTerm<?> arg,
                                                 TypedTerm<?> defaultBranch, Field... branches) {
        Term match = Terms.match(typeName.value, Optional.<Term>given(defaultBranch.value), branches);
        return tterm(Terms.apply(match, arg.value));
    }

    /** {@code match TypeName Nothing [branches]} — match function (not yet applied). */
    public static <A> TypedTerm<A> match(String typeName, Field... branches) {
        return tterm(Terms.match(typeName, Optional.<Term>none(), branches));
    }

    /** {@code match TypeName Nothing [branches]} — Name overload. */
    public static <A> TypedTerm<A> match(Name typeName, Field... branches) {
        return tterm(Terms.match(typeName.value, Optional.<Term>none(), branches));
    }

    /** {@code match TypeName (Just default) [branches]}. */
    public static <A> TypedTerm<A> matchWithDefault(String typeName, TypedTerm<?> defaultBranch,
                                                 Field... branches) {
        return tterm(Terms.match(typeName, Optional.<Term>given(defaultBranch.value), branches));
    }

    /** {@code match TypeName (Just default) [branches]} — Name overload. */
    public static <A> TypedTerm<A> matchWithDefault(Name typeName, TypedTerm<?> defaultBranch,
                                                 Field... branches) {
        return tterm(Terms.match(typeName.value, Optional.<Term>given(defaultBranch.value), branches));
    }

    // ---- Type application / type lambda ----

    /** {@code term :: forall ts. body} — apply a type argument. */
    public static <A> TypedTerm<A> tyapp(TypedTerm<?> term, Type type) {
        return tterm(Terms.tyapp(term.value, type));
    }

    /** {@code tyapps term [t1, t2, ...]} — apply multiple type arguments. */
    public static <A> TypedTerm<A> tyapps(TypedTerm<?> term, Type... types) {
        Term r = term.value;
        for (Type t : types) {
            r = Terms.tyapp(r, t);
        }
        return tterm(r);
    }

    /** {@code typeLambda [v1, v2, ...] body}. */
    public static <A> TypedTerm<A> typeLambda(List<String> vars, TypedTerm<?> body) {
        List<Name> names = new java.util.ArrayList<>();
        for (String v : vars) names.add(new Name(v));
        return tterm(Terms.typeLambda(names, body.value));
    }

    /** Single-variable type lambda. */
    public static <A> TypedTerm<A> typeLambda(String var, TypedTerm<?> body) {
        return typeLambda(Collections.singletonList(var), body);
    }

    // ---- Annotations / documentation ----

    /** Attach a description annotation to a term. */
    public static <A> TypedTerm<A> doc(String description, TypedTerm<A> term) {
        return tterm(Terms.annot(description, term.value));
    }

    // ---- Definitions ----

    /** Build a TypedBinding for a definition in a module. */
    public static <A> TypedBinding<A> definitionInModule(Module mod, String localName, TypedTerm<A> term) {
        Name fqName = new Name(mod.name.value + "." + localName);
        return new TypedBinding<A>(fqName, term);
    }

    /** Convert a TypedBinding to a Definition (without explicit type scheme). */
    public static <A> Definition toDefinition(TypedBinding<A> tb) {
        return new Definition.Term(new TermDefinition(
            tb.name,
            Optional.<EntityMetadata>none(),
            Optional.<TermSignature>none(),
            tb.term.value));
    }

    /** Build a Definition directly: namespace + localName + term, no TypedBinding. */
    public static <A> Definition def(ModuleName ns, String localName, TypedTerm<A> term) {
        Name fqName = new Name(ns.value + "." + localName);
        return new Definition.Term(new TermDefinition(
            fqName,
            Optional.<EntityMetadata>none(),
            Optional.<TermSignature>none(),
            term.value));
    }

    /** Build a Definition with a pre-computed TypeScheme (used for hydra.java.coder). */
    public static <A> Definition defTyped(ModuleName ns, String localName, TypedTerm<A> term,
                                            TypeScheme ts) {
        Name fqName = new Name(ns.value + "." + localName);
        return new Definition.Term(new TermDefinition(
            fqName,
            Optional.<EntityMetadata>none(),
            Optional.<TermSignature>given(Scoping.typeSchemeToTermSignature(ts)),
            term.value));
    }
}
