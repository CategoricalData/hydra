package hydra.dsl.meta;

import hydra.core.Application;
import hydra.core.Binding;
import hydra.core.CaseStatement;
import hydra.core.Elimination;
import hydra.core.Field;
import hydra.core.Function;
import hydra.core.Injection;
import hydra.core.Lambda;
import hydra.core.Let;
import hydra.core.Literal;
import hydra.core.Name;
import hydra.core.Projection;
import hydra.core.Record;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.WrappedTerm;
import hydra.module.Namespace;
import hydra.phantoms.TBinding;
import hydra.phantoms.TTerm;
import hydra.util.Maybe;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Phantom-typed term construction DSL for Hydra.
 *
 * <p>Mirrors Hydra.Dsl.Meta.Phantoms (Haskell), wrapping the raw Terms DSL
 * with {@link Expr}&lt;A&gt; phantom types for compile-time type safety.
 *
 * <p>All methods return {@link Expr} (which extends {@link TTerm}) so that
 * the {@link Expr#of(TTerm)} chaining operator is available on every result.
 *
 * <p>Usage: {@code import static hydra.dsl.meta.Phantoms.*;}
 */
public interface Phantoms {

    // ===== Unwrapping =====

    /**
     * Extract the underlying Term from a TTerm.
     */
    static <A> Term unTTerm(TTerm<A> t) {
        return t.value;
    }

    /**
     * Wrap a raw Term in an Expr.
     */
    static <A> Expr<A> tterm(Term t) {
        return new Expr<>(t);
    }

    // ===== Core Operations =====

    /**
     * Apply a function to an argument.
     * Uses wildcards because phantom types are erased at runtime;
     * the return type is always Expr with the caller's expected phantom.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> apply(TTerm<?> fun, TTerm<?> arg) {
        return new Expr<>(new Term.Application(new Application(fun.value, arg.value)));
    }

    /**
     * Create a lambda function with one parameter.
     */
    @SuppressWarnings("unchecked")
    static <A, B> Expr<B> lambda(String param, TTerm<A> body) {
        return new Expr<>(new Term.Function(new Function.Lambda(
                new Lambda(new Name(param), Maybe.nothing(), body.value))));
    }

    /**
     * Create a multi-parameter lambda function.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> lambdas(List<String> params, TTerm<?> body) {
        Term result = body.value;
        for (int i = params.size() - 1; i >= 0; i--) {
            result = new Term.Function(new Function.Lambda(
                    new Lambda(new Name(params.get(i)), Maybe.nothing(), result)));
        }
        return new Expr<>(result);
    }

    /**
     * Create a variable reference.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> var(String name) {
        return new Expr<>(new Term.Variable(new Name(name)));
    }

    /**
     * Unit value (empty record).
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> unit() {
        return new Expr<>(new Term.Unit());
    }

    // ===== Literal Constructors =====

    /**
     * Create a string literal.
     */
    static Expr<String> string(String value) {
        return new Expr<>(hydra.dsl.Terms.string(value));
    }

    /**
     * Create an int32 literal.
     */
    static Expr<Integer> int32(int value) {
        return new Expr<>(hydra.dsl.Terms.int32(value));
    }

    /**
     * Create a boolean literal.
     */
    static Expr<Boolean> boolean_(boolean value) {
        return new Expr<>(hydra.dsl.Terms.boolean_(value));
    }

    /**
     * Boolean true literal.
     */
    static Expr<Boolean> true_() {
        return boolean_(true);
    }

    /**
     * Boolean false literal.
     */
    static Expr<Boolean> false_() {
        return boolean_(false);
    }

    // ===== Record Construction =====

    /**
     * Create a record with named fields.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> record(Name typeName, List<Field> fields) {
        return new Expr<>(new Term.Record(new Record(typeName, fields)));
    }

    /**
     * Create a record with named fields (varargs).
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> record(Name typeName, Field... fields) {
        return record(typeName, Arrays.asList(fields));
    }

    /**
     * Create a field with the given name and value.
     */
    static <A> Field field(Name fname, TTerm<A> val) {
        return new Field(fname, val.value);
    }

    // ===== Union/Injection =====

    /**
     * Create a union injection.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> inject(Name typeName, Name fieldName, TTerm<?> term) {
        return new Expr<>(new Term.Union(new Injection(typeName, new Field(fieldName, term.value))));
    }

    /**
     * Create a unit injection of a union.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> injectUnit(Name typeName, Name fieldName) {
        return new Expr<>(new Term.Union(new Injection(typeName, new Field(fieldName, new Term.Unit()))));
    }

    // ===== Projection =====

    /**
     * Create a record projection (field accessor).
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> project(Name recordName, Name fieldName) {
        return new Expr<>(new Term.Function(new Function.Elimination(
                new Elimination.Record(new Projection(recordName, fieldName)))));
    }

    // ===== Pattern Matching =====

    /**
     * Create a pattern match on a union term.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> match(Name typeName, Maybe<TTerm<?>> dflt, List<Field> fields) {
        Maybe<Term> dfltTerm = dflt.map(t -> t.value);
        return new Expr<>(new Term.Function(new Function.Elimination(
                new Elimination.Union(new CaseStatement(typeName, dfltTerm, fields)))));
    }

    /**
     * Create a pattern match on a union term (varargs fields).
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> match(Name typeName, Maybe<TTerm<?>> dflt, Field... fields) {
        return match(typeName, dflt, Arrays.asList(fields));
    }

    /**
     * Apply a named case match to an argument.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> cases(Name typeName, TTerm<?> arg, Maybe<TTerm<?>> dflt, List<Field> fields) {
        TTerm<?> matchTerm = match(typeName, dflt, fields);
        return new Expr<>(new Term.Application(new Application(matchTerm.value, arg.value)));
    }

    /**
     * Apply a named case match to an argument (varargs fields).
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> cases(Name typeName, TTerm<?> arg, Maybe<TTerm<?>> dflt, Field... fields) {
        return cases(typeName, arg, dflt, Arrays.asList(fields));
    }

    // ===== Let Bindings =====

    /**
     * Create a let expression with a single binding.
     */
    @SuppressWarnings("unchecked")
    static <A, B> Expr<B> let(String name, TTerm<A> value, TTerm<B> body) {
        List<Binding> bindings = Collections.singletonList(
                new Binding(new Name(name), value.value, Maybe.nothing()));
        return new Expr<>(new Term.Let(new Let(bindings, body.value)));
    }

    /**
     * Create a let expression with multiple bindings.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> lets(List<Field> fields, TTerm<A> body) {
        List<Binding> bindings = new ArrayList<>();
        for (Field f : fields) {
            bindings.add(new Binding(f.name, f.term, Maybe.nothing()));
        }
        return new Expr<>(new Term.Let(new Let(bindings, body.value)));
    }

    // ===== Composition =====

    /**
     * Compose two functions (g then f).
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> compose(TTerm<?> f, TTerm<?> g) {
        return new Expr<>(hydra.dsl.Terms.compose(f.value, g.value));
    }

    /**
     * Create a constant function that always returns the same value.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> constant(TTerm<?> value) {
        return new Expr<>(hydra.dsl.Terms.constant(value.value));
    }

    /**
     * Identity function.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> identity() {
        return new Expr<>(hydra.dsl.Terms.identity());
    }

    // ===== Optional/Either =====

    /**
     * Create a 'Just' optional value.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> just(TTerm<?> term) {
        return new Expr<>(hydra.dsl.Terms.just(term.value));
    }

    /**
     * Create a 'Nothing' optional value.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> nothing() {
        return new Expr<>(hydra.dsl.Terms.nothing());
    }

    /**
     * Create a 'Left' either value.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> left(TTerm<?> term) {
        return new Expr<>(hydra.dsl.Terms.left(term.value));
    }

    /**
     * Create a 'Right' either value.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> right(TTerm<?> term) {
        return new Expr<>(hydra.dsl.Terms.right(term.value));
    }

    // ===== Collections =====

    /**
     * Create a list of terms.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<List<A>> list(List<TTerm<A>> elements) {
        List<Term> terms = new ArrayList<>();
        for (TTerm<A> el : elements) {
            terms.add(el.value);
        }
        return new Expr<>(hydra.dsl.Terms.list(terms));
    }

    /**
     * Create a list of terms (varargs).
     */
    @SafeVarargs
    @SuppressWarnings("unchecked")
    static <A> Expr<List<A>> list(TTerm<A>... elements) {
        return list(Arrays.asList(elements));
    }

    /**
     * Create a pair.
     */
    @SuppressWarnings("unchecked")
    static <A, B> Expr<Object> pair(TTerm<A> first, TTerm<B> second) {
        return new Expr<>(hydra.dsl.Terms.pair(first.value, second.value));
    }

    /**
     * Create a set of terms.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<Set<A>> set(List<TTerm<A>> elements) {
        java.util.Set<Term> terms = new java.util.LinkedHashSet<>();
        for (TTerm<A> el : elements) {
            terms.add(el.value);
        }
        return new Expr<>(hydra.dsl.Terms.set(terms));
    }

    // ===== Wrap/Unwrap =====

    /**
     * Create a wrapped term (instance of a newtype).
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> wrap(Name wrapName, TTerm<?> term) {
        return new Expr<>(hydra.dsl.Terms.wrap(wrapName, term.value));
    }

    /**
     * Create an unwrap function for a wrapped type.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> unwrap(Name wrapName) {
        return new Expr<>(hydra.dsl.Terms.unwrap(wrapName));
    }

    // ===== Primitive Functions =====

    /**
     * Reference a primitive function by name.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> primitive(Name primName) {
        return new Expr<>(new Term.Function(new Function.Primitive(primName)));
    }

    /**
     * Apply a primitive function to one argument.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> primitive1(Name primName, TTerm<?> a) {
        return new Expr<>(new Term.Application(new Application(
                new Term.Function(new Function.Primitive(primName)), a.value)));
    }

    /**
     * Apply a primitive function to two arguments.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> primitive2(Name primName, TTerm<?> a, TTerm<?> b) {
        Term inner = new Term.Application(new Application(
                new Term.Function(new Function.Primitive(primName)), a.value));
        return new Expr<>(new Term.Application(new Application(inner, b.value)));
    }

    /**
     * Apply a primitive function to three arguments.
     */
    @SuppressWarnings("unchecked")
    static <R> Expr<R> primitive3(Name primName, TTerm<?> a, TTerm<?> b, TTerm<?> c) {
        Term inner1 = new Term.Application(new Application(
                new Term.Function(new Function.Primitive(primName)), a.value));
        Term inner2 = new Term.Application(new Application(inner1, b.value));
        return new Expr<>(new Term.Application(new Application(inner2, c.value)));
    }

    // ===== Documentation =====

    /**
     * Add documentation to a term.
     */
    static <A> Expr<A> doc(String description, TTerm<A> term) {
        return new Expr<>(hydra.dsl.Terms.annot(description, term.value));
    }

    // ===== Binding Helpers =====

    /**
     * Create a definition in a namespace.
     * This mirrors Haskell's {@code definitionInNamespace} and Python's {@code definition_in_namespace}.
     *
     * @param ns the namespace
     * @param localName the local name within the namespace
     * @param term the term to bind
     * @return a typed binding
     */
    static <A> TBinding<A> definitionInNamespace(Namespace ns, String localName, TTerm<A> term) {
        return new TBinding<>(new Name(ns.value + "." + localName), term);
    }

    /**
     * Convert a TBinding to an untyped Binding.
     */
    static <A> Binding toBinding(TBinding<A> tb) {
        return new Binding(tb.name, tb.term.value, Maybe.nothing());
    }

    /**
     * Reference a binding as a variable.
     */
    @SuppressWarnings("unchecked")
    static <A> Expr<A> ref(TBinding<A> binding) {
        return new Expr<>(new Term.Variable(binding.name));
    }

    // ===== Fluent Builders =====

    /**
     * Create a lambda (2-arg shorthand for {@link #lambda(String, TTerm)}).
     */
    @SuppressWarnings("unchecked")
    static <A, B> Expr<B> lam(String param, TTerm<A> body) {
        return lambda(param, body);
    }

    /**
     * Start a fluent lambda chain (1-arg overload).
     * Use: {@code lam("x").to(body)} instead of {@code lambda("x", body)}.
     */
    static ExprBuilder lam(String param) {
        return new ExprBuilder(java.util.Collections.singletonList(Intro.lambda(param)));
    }

    /**
     * Start a fluent multi-parameter lambda chain.
     * Use: {@code lams("x", "y").to(body)} instead of {@code lambda("x", lambda("y", body))}.
     */
    static ExprBuilder lams(String... params) {
        List<Intro> intros = new ArrayList<>();
        for (String p : params) {
            intros.add(Intro.lambda(p));
        }
        return new ExprBuilder(intros);
    }

    /**
     * Start a fluent definition in a namespace (returning a DefineBuilder).
     * Use: {@code define(ns, "name").doc("...").lam("x").to(body)}.
     */
    static DefineBuilder define(Namespace ns, String localName) {
        return new DefineBuilder(new Name(ns.value + "." + localName));
    }

    /**
     * Create a local reference function for a namespace.
     * Returns a function that takes a local name and produces an Expr variable reference
     * with the full qualified name.
     *
     * <p>Usage:
     * <pre>
     *   java.util.function.Function&lt;String, Expr&lt;?&gt;&gt; local = makeLocal(ns);
     *   ...local.apply("rewriteTerm")...
     * </pre>
     */
    static java.util.function.Function<String, Expr<?>> makeLocal(Namespace ns) {
        String prefix = ns.value + ".";
        return localName -> var(prefix + localName);
    }
}
