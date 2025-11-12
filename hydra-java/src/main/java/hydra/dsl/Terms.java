package hydra.dsl;

import hydra.compute.FlowState;
import hydra.core.*;
import hydra.util.Comparison;
import hydra.util.Opt;

import java.math.BigInteger;
import java.util.*;

import static hydra.dsl.Core.name;
import static hydra.dsl.Literals.*;

/**
 * A domain-specific language for constructing Hydra terms in Java.
 * Provides parity with Hydra.Dsl.Terms (Haskell) and hydra.dsl.terms (Python).
 */
public interface Terms {
    String IGNORED_VARIABLE = "_";

    // ===== Term annotations =====

    /**
     * Attach an annotation to a term.
     * Example: annot(Map.of(name("comment"), string("A user ID")), var("userId"))
     */
    static Term annot(Map<Name, Term> ann, Term base) {
        return new Term.Annotated(new AnnotatedTerm(base, ann));
    }

    /**
     * Attach an annotation to a term (alternative argument order).
     */
    static Term annotated(Term base, Map<Name, Term> ann) {
        return annot(ann, base);
    }

    /**
     * Attach an annotation with a single key/value pair.
     */
    static Term annot(Name key, Term value, Term base) {
        Map<Name, Term> mp = new HashMap<>();
        mp.put(key, value);
        return annot(mp, base);
    }

    /**
     * Attach an annotation with "description" key.
     */
    static Term annot(String description, Term base) {
        return annot(name("description"), string(description), base);
    }

    // ===== Application =====

    /**
     * Apply a function term to an argument.
     * Example: apply(var("capitalize"), string("arthur"))
     */
    static Term apply(Term func, Term arg) {
        return new Term.Application(new Application(func, arg));
    }

    /**
     * Apply a function to multiple arguments.
     * Example: apply(var("add"), int32(1), int32(2))
     */
    static Term apply(Term func, Term... args) {
        Term result = func;
        for (Term arg : args) {
            result = apply(result, arg);
        }
        return result;
    }

    /**
     * Apply a function to multiple arguments.
     */
    static Term applyAll(Term func, List<Term> args) {
        Term result = func;
        for (Term arg : args) {
            result = apply(result, arg);
        }
        return result;
    }

    /**
     * Alias for apply.
     */
    static Term app(Term func, Term... args) {
        return apply(func, args);
    }

    /**
     * Apply a variable to arguments.
     */
    static Term app(String func, Term... args) {
        return apply(var(func), args);
    }

    // ===== Literal terms (delegating to Literals.java for Java-specific mappings) =====

    /**
     * Construct a term from a literal value.
     */
    static Term literal(Literal value) {
        return new Term.Literal(value);
    }

    /**
     * Create a binary data literal.
     * Example: binary("\\x48\\x65\\x00\\xff")
     */
    static Term binary(String value) {
        return literal(Literals.binary(value));
    }

    /**
     * Create a boolean literal.
     */
    static Term boolean_(boolean value) {
        return literal(Literals.boolean_(value));
    }

    /**
     * Boolean false literal.
     */
    static Term false_() {
        return boolean_(false);
    }

    /**
     * Boolean true literal.
     */
    static Term true_() {
        return boolean_(true);
    }

    /**
     * Create a string literal.
     */
    static Term string(String value) {
        return literal(Literals.string(value));
    }

    // ===== Integer terms (Java-specific primitive mappings preserved) =====

    /**
     * Create an integer literal with specified bit width.
     */
    static Term integer(IntegerValue value) {
        return literal(Literals.integer(value));
    }

    /**
     * Create an int8 literal (Java byte → Hydra int8).
     */
    static Term int8(byte value) {
        return literal(Literals.int8(value));
    }

    /**
     * Create an int16 literal (Java short → Hydra int16).
     */
    static Term int16(short value) {
        return literal(Literals.int16(value));
    }

    /**
     * Create an int32 literal (Java int → Hydra int32).
     */
    static Term int32(int value) {
        return literal(Literals.int32(value));
    }

    /**
     * Create an int64 literal (Java long → Hydra int64).
     */
    static Term int64(long value) {
        return literal(Literals.int64(value));
    }

    /**
     * Create a bigint literal (Java BigInteger → Hydra bigint).
     */
    static Term bigint(BigInteger value) {
        return literal(Literals.bigint(value));
    }

    /**
     * Create a uint8 literal (Java char → Hydra uint8).
     */
    static Term uint8(char value) {
        return literal(Literals.uint8(value));
    }

    /**
     * Create a uint16 literal (Java char → Hydra uint16).
     */
    static Term uint16(char value) {
        return literal(Literals.uint16(value));
    }

    /**
     * Create a uint32 literal (Java long → Hydra uint32).
     */
    static Term uint32(long value) {
        return literal(Literals.uint32(value));
    }

    /**
     * Create a uint64 literal (Java BigInteger → Hydra uint64).
     */
    static Term uint64(BigInteger value) {
        return literal(Literals.uint64(value));
    }

    // ===== Floating point terms (Java-specific mappings preserved) =====

    /**
     * Create a floating-point literal with specified precision.
     */
    static Term float_(FloatValue value) {
        return literal(Literals.float_(value));
    }

    /**
     * Create a float32 literal (Java float → Hydra float32).
     */
    static Term float32(float value) {
        return literal(Literals.float32(value));
    }

    /**
     * Create a float64 literal (Java double → Hydra float64).
     */
    static Term float64(double value) {
        return literal(Literals.float64(value));
    }

    /**
     * Create a bigfloat literal (Java double → Hydra bigfloat).
     */
    static Term bigfloat(double value) {
        return literal(Literals.bigfloat(value));
    }

    // ===== Character and comparison =====

    /**
     * Create a character term as an int32 (ord value).
     */
    static Term char_(char value) {
        return int32((int) value);
    }

    /**
     * Create a comparison term.
     */
    static Term comparison(Comparison comp) {
        return comp.accept(new Comparison.Visitor<Term>() {
            @Override
            public Term visit(Comparison.EqualTo instance) {
                return unitVariant(name("hydra.mantle.Comparison"), name("equalTo"));
            }

            @Override
            public Term visit(Comparison.LessThan instance) {
                return unitVariant(name("hydra.mantle.Comparison"), name("lessThan"));
            }

            @Override
            public Term visit(Comparison.GreaterThan instance) {
                return unitVariant(name("hydra.mantle.Comparison"), name("greaterThan"));
            }
        });
    }

    // ===== Function terms =====

    /**
     * Construct a function term.
     */
    static Term function(Function func) {
        return new Term.Function(func);
    }

    /**
     * Create a lambda function with one parameter.
     * Example: lambda("x", apply(var("x"), int32(1)))
     */
    static Term lambda(String param, Term body) {
        return function(new Function.Lambda(new Lambda(name(param), Opt.empty(), body)));
    }

    /**
     * Create a multi-parameter lambda function (curried).
     * Example: lambdas(Arrays.asList("x", "y"), apply(var("add"), var("x"), var("y")))
     */
    static Term lambdas(List<String> params, Term body) {
        if (params.isEmpty()) {
            return body;
        }
        Term result = body;
        for (int i = params.size() - 1; i >= 0; i--) {
            result = lambda(params.get(i), result);
        }
        return result;
    }

    /**
     * Create a lambda function with varargs parameters.
     */
    static Term lambdas(String[] params, Term body) {
        return lambdas(Arrays.asList(params), body);
    }

    /**
     * Create a lambda with two parameters.
     */
    static Term lambda(String param1, String param2, Term body) {
        return lambda(param1, lambda(param2, body));
    }

    /**
     * Create a lambda with three parameters.
     */
    static Term lambda(String param1, String param2, String param3, Term body) {
        return lambda(param1, lambda(param2, lambda(param3, body)));
    }

    /**
     * Create a lambda function with a given domain type.
     * Example: lambdaTyped("x", Types.int32(), list(var("x")))
     */
    static Term lambdaTyped(String param, Type dom, Term body) {
        return function(new Function.Lambda(new Lambda(name(param), Opt.of(dom), body)));
    }

    /**
     * Identity function.
     */
    static Term identity() {
        return lambda("x_", var("x_"));
    }

    /**
     * Create a constant function that always returns the same value.
     */
    static Term constant(Term value) {
        return lambda(IGNORED_VARIABLE, value);
    }

    /**
     * Compose two functions (apply g then f).
     * Example: compose(var("stringLength"), var("toString"))
     */
    static Term compose(Term f, Term g) {
        return lambda("arg_", apply(f, apply(g, var("arg_"))));
    }

    /**
     * Create a primitive function.
     * Example: primitive(name("hydra.lib.strings.length"))
     */
    static Term primitive(Name primName) {
        return function(new Function.Primitive(primName));
    }

    /**
     * Create a primitive function.
     */
    static Term primitive(String primName) {
        return primitive(name(primName));
    }

    // ===== Elimination terms =====

    /**
     * Construct an elimination term.
     */
    static Term elimination(Elimination elim) {
        return function(new Function.Elimination(elim));
    }

    /**
     * Create a projection term for accessing record fields.
     * Example: project(name("Person"), name("firstName"))
     */
    static Term project(Name recordName, Name fieldName) {
        return elimination(new Elimination.Record(new Projection(recordName, fieldName)));
    }

    /**
     * Create a projection term.
     */
    static Term project(String recordName, String fieldName) {
        return project(name(recordName), name(fieldName));
    }

    /**
     * Project a field from a record (with Name recordName and String fieldName).
     */
    static Term project(Name recordName, String fieldName) {
        return project(recordName, name(fieldName));
    }

    /**
     * Create an unwrap function for a wrapped type.
     * Example: unwrap(name("Email"))
     */
    static Term unwrap(Name wrapName) {
        return elimination(new Elimination.Wrap(wrapName));
    }

    /**
     * Create an unwrap function.
     */
    static Term unwrap(String wrapName) {
        return unwrap(name(wrapName));
    }

    // TODO: restore when Elimination.List is generated
    // /**
    //  * Create a fold term for lists.
    //  */
    // static Term fold(Term func) {
    //     return elimination(new Elimination.List(func));
    // }

    /**
     * Create a tuple projection function.
     * Example: untuple(3, 1) extracts the second element of a 3-tuple
     */
    static Term untuple(int arity, int idx) {
        return elimination(new Elimination.Product(new TupleProjection(arity, idx, Opt.empty())));
    }

    /**
     * Create a tuple projection function with explicit types.
     */
    static Term untuple(int arity, int idx, Opt<List<Type>> types) {
        return elimination(new Elimination.Product(new TupleProjection(arity, idx, types)));
    }

    /**
     * First element projection function for pairs.
     */
    static Term first() {
        return untuple(2, 0);
    }

    /**
     * Second element projection function for pairs.
     */
    static Term second() {
        return untuple(2, 1);
    }

    /**
     * Create a pattern match on a union type.
     * Example: match(name("Result"), Opt.of(string("unknown")),
     *               field("success", lambda("s", apply(var("processSuccess"), var("s")))),
     *               field("error", lambda("e", apply(var("handleError"), var("e")))))
     */
    static Term match(Name typeName, Opt<Term> defaultCase, Field... fields) {
        return elimination(new Elimination.Union(new CaseStatement(typeName, defaultCase, Arrays.asList(fields))));
    }

    /**
     * Create a pattern match on a union type.
     */
    static Term match(Name typeName, Opt<Term> defaultCase, List<Field> fields) {
        return elimination(new Elimination.Union(new CaseStatement(typeName, defaultCase, fields)));
    }

    /**
     * Create a pattern match on a union type (with string type name).
     */
    static Term match(String typeName, Opt<Term> defaultCase, Field... fields) {
        return match(name(typeName), defaultCase, fields);
    }

    // ===== Let bindings =====

    /**
     * Create a let term with a single binding.
     * Example: let_("x", int32(1), var("x"))
     */
    static Term let_(String varName, Term defined, Term body) {
        Binding binding = new Binding(name(varName), defined, Opt.empty());
        return new Term.Let(new Let(Collections.singletonList(binding), body));
    }

    /**
     * Create a let term with any number of bindings.
     * Example: lets(Arrays.asList(field("x", int32(1)), field("y", int32(2))), pair(var("x"), var("y")))
     */
    static Term lets(List<Field> bindings, Term body) {
        List<Binding> letBindings = new ArrayList<>();
        for (Field f : bindings) {
            letBindings.add(new Binding(f.name, f.term, Opt.empty()));
        }
        return new Term.Let(new Let(letBindings, body));
    }

    /**
     * Create a let term with typed bindings.
     */
    static Term letsTyped(List<Binding> bindings, Term body) {
        return new Term.Let(new Let(bindings, body));
    }

    // ===== Field construction =====

    /**
     * Create a field with the given name and value.
     * Example: field("age", int32(30))
     */
    static Field field(String name, Term term) {
        return new Field(name(name), term);
    }

    /**
     * Create a field.
     */
    static Field field(Name name, Term term) {
        return new Field(name, term);
    }

    /**
     * Create a field (with variable term).
     */
    static Field field(String name, String varName) {
        return field(name, var(varName));
    }

    // ===== Collection terms =====

    /**
     * Create a list of terms.
     * Example: list(int32(1), int32(2), int32(3))
     */
    static Term list(Term... elements) {
        return new Term.List(Arrays.asList(elements));
    }

    /**
     * Create a list of terms.
     */
    static Term list(List<Term> elements) {
        return new Term.List(elements);
    }

    /**
     * Create a list of strings.
     */
    static Term listOfStrings(List<String> elements) {
        List<Term> terms = new ArrayList<>();
        for (String s : elements) {
            terms.add(string(s));
        }
        return list(terms);
    }

    /**
     * Create a map/dictionary term.
     * Example: map(Map.of(string("January"), int32(31), string("February"), int32(28)))
     */
    static Term map(Map<Term, Term> value) {
        return new Term.Map(value);
    }

    /**
     * Create a set of terms.
     * Example: set(Set.of(string("a"), string("b"), string("c")))
     */
    static Term set(Set<Term> value) {
        return new Term.Set(value);
    }

    // ===== Optional terms =====

    /**
     * Create an optional (nullable) term.
     */
    static Term optional(Opt<Term> maybeTerm) {
        return new Term.Maybe(maybeTerm);
    }

    /**
     * Create a 'Nothing' optional value.
     */
    static Term nothing() {
        return optional(Opt.empty());
    }

    /**
     * Create a 'Just' optional value.
     * Example: just(string("found"))
     */
    static Term just(Term elem) {
        return optional(Opt.of(elem));
    }

    /**
     * Create a 'Just' optional value (with variable).
     */
    static Term just(String varName) {
        return just(var(varName));
    }

    // ===== Product terms =====

    /**
     * Create a pair.
     * Example: pair(string("name"), int32(42))
     */
    static Term pair(Term a, Term b) {
        return new Term.Product(Arrays.asList(a, b));
    }

    /**
     * Create a 2-tuple (alias for pair).
     */
    static Term tuple2(Term a, Term b) {
        return new Term.Product(Arrays.asList(a, b));
    }

    /**
     * Create a product (tuple) with multiple components.
     * Example: tuple(string("name"), int32(42), true_())
     */
    static Term tuple(Term... elements) {
        return new Term.Product(Arrays.asList(elements));
    }

    /**
     * Create a product term.
     */
    static Term tuple(List<Term> elements) {
        return new Term.Product(elements);
    }

    /**
     * Create a product term.
     */
    static Term product(Term... elements) {
        return tuple(elements);
    }

    /**
     * Create a product term.
     */
    static Term product(List<Term> elements) {
        return tuple(elements);
    }

    /**
     * Create a triple.
     */
    static Term triple(Term a, Term b, Term c) {
        return tuple(a, b, c);
    }

    /**
     * Create a 4-tuple.
     */
    static Term tuple4(Term a, Term b, Term c, Term d) {
        return tuple(a, b, c, d);
    }

    /**
     * Create a 5-tuple.
     */
    static Term tuple5(Term a, Term b, Term c, Term d, Term e) {
        return tuple(a, b, c, d, e);
    }

    // ===== Sum terms =====

    /**
     * Create a sum term.
     * Example: sum(0, 3, int32(1)) represents the first element of a 3-element sum
     */
    static Term sum(int idx, int arity, Term term) {
        return new Term.Sum(new Sum(idx, arity, term));
    }

    /**
     * Create a 'Left' either value.
     * Example: left(string("error"))
     */
    static Term left(Term term) {
        return sum(0, 2, term);
    }

    /**
     * Create a 'Right' either value.
     * Example: right(int32(42))
     */
    static Term right(Term term) {
        return sum(1, 2, term);
    }

    // ===== Record terms =====

    /**
     * Create a record with named fields.
     * Example: record(name("Person"),
     *            field("name", string("John")),
     *            field("age", int32(30)))
     */
    static Term record(Name typeName, Field... fields) {
        return new Term.Record(new Record(typeName, Arrays.asList(fields)));
    }

    /**
     * Create a record term.
     */
    static Term record(Name typeName, List<Field> fields) {
        return new Term.Record(new Record(typeName, fields));
    }

    /**
     * Create a record term (with string type name).
     */
    static Term record(String typeName, Field... fields) {
        return record(name(typeName), fields);
    }

    /**
     * Unit value (empty record).
     */
    static Term unit() {
        return record(name("_Unit"));
    }

    // ===== Union terms =====

    /**
     * Create a union value by injecting a value into a specific variant.
     * Example: inject(name("Result"), field("success", int32(42)))
     */
    static Term inject(Name typeName, Field field) {
        return new Term.Union(new Injection(typeName, field));
    }

    /**
     * Create an injection term.
     */
    static Term inject(String typeName, Field field) {
        return inject(name(typeName), field);
    }

    /**
     * Create a union variant.
     * Example: variant(name("Result"), name("success"), string("ok"))
     */
    static Term variant(Name typeName, Name fieldName, Term term) {
        return inject(typeName, new Field(fieldName, term));
    }

    /**
     * Create a union variant.
     */
    static Term variant(String typeName, String fieldName, Term term) {
        return variant(name(typeName), name(fieldName), term);
    }

    /**
     * Create a unit variant of a union.
     * Example: unitVariant(name("Result"), name("success"))
     */
    static Term unitVariant(Name typeName, Name fieldName) {
        return variant(typeName, fieldName, unit());
    }

    /**
     * Create a unit variant.
     */
    static Term unitVariant(String typeName, String fieldName) {
        return unitVariant(name(typeName), name(fieldName));
    }

    // ===== Wrapped terms =====

    /**
     * Create a wrapped term.
     * Example: wrap(name("Email"), string("user@example.com"))
     */
    static Term wrap(Name wrapName, Term term) {
        return new Term.Wrap(new WrappedTerm(wrapName, term));
    }

    /**
     * Create a wrapped term.
     */
    static Term wrap(String wrapName, Term term) {
        return wrap(name(wrapName), term);
    }

    // ===== Variables =====

    /**
     * Create a variable reference.
     * Example: var("x")
     */
    static Term var(String name) {
        return new Term.Variable(name(name));
    }

    /**
     * Create a variable reference.
     */
    static Term variable(Name name) {
        return new Term.Variable(name);
    }

    /**
     * Create a variable reference.
     */
    static Term variable(String name) {
        return var(name);
    }

    // ===== Type abstraction and application =====

    /**
     * Create a type lambda (type abstraction).
     * Example: tylam("a", lambda("x", var("x")))
     */
    static Term tylam(String var, Term body) {
        return new Term.TypeLambda(new TypeLambda(name(var), body));
    }

    /**
     * Create a type lambda with multiple variables.
     */
    static Term tylams(List<String> vars, Term body) {
        Term result = body;
        for (int i = vars.size() - 1; i >= 0; i--) {
            result = tylam(vars.get(i), result);
        }
        return result;
    }

    /**
     * Create a type lambda with multiple variables.
     */
    static Term tylams(String[] vars, Term body) {
        return tylams(Arrays.asList(vars), body);
    }

    /**
     * Create a type abstraction (universal quantification).
     */
    static Term typeLambda(List<Name> vars, Term body) {
        Term result = body;
        for (Name var : vars) {
            result = new Term.TypeLambda(new TypeLambda(var, result));
        }
        return result;
    }

    /**
     * Apply a type argument to a polymorphic term.
     * Example: tyapp(var("map"), Types.int32())
     */
    static Term tyapp(Term term, Type typ) {
        return new Term.TypeApplication(new TypeApplicationTerm(term, typ));
    }

    /**
     * Apply type arguments to a polymorphic term.
     * Example: tyapps(var("map"), Arrays.asList(Types.int32(), Types.string()))
     */
    static Term tyapps(Term term, List<Type> types) {
        Term result = term;
        for (Type typ : types) {
            result = tyapp(result, typ);
        }
        return result;
    }

    /**
     * Apply type arguments to a polymorphic term.
     */
    static Term tyapps(Term term, Type... types) {
        return tyapps(term, Arrays.asList(types));
    }

    /**
     * Apply type arguments to a polymorphic term (alias).
     */
    static Term typeApplication(Term term, List<Type> types) {
        return tyapps(term, types);
    }

    // ===== Flow monad helpers =====

    /**
     * Construct a flow state term (used with the Flow monad).
     */
    static Term flowState(Term value, Term state, Term trace) {
        return record(FlowState.TYPE_NAME,
                field("value", value),
                field("state", state),
                field("trace", trace));
    }

    /**
     * Construct a value projection (used with the Flow monad).
     */
    static Term flowStateValue() {
        return project(FlowState.TYPE_NAME, name("value"));
    }

    /**
     * Construct a state projection (used with the Flow monad).
     */
    static Term flowStateState() {
        return project(FlowState.TYPE_NAME, name("state"));
    }

    /**
     * Construct a trace projection (used with the Flow monad).
     */
    static Term flowStateTrace() {
        return project(FlowState.TYPE_NAME, name("trace"));
    }
}
