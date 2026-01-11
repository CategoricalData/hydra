package hydra.dsl;

import hydra.compute.FlowState;
import hydra.core.AnnotatedTerm;
import hydra.core.Application;
import hydra.core.Binding;
import hydra.core.CaseStatement;
import hydra.core.Elimination;
import hydra.core.Field;
import hydra.core.FloatValue;
import hydra.core.Function;
import hydra.core.Injection;
import hydra.core.IntegerValue;
import hydra.core.Lambda;
import hydra.core.Let;
import hydra.core.Literal;
import hydra.core.Name;
import hydra.core.Projection;
import hydra.core.Record;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeApplicationTerm;
import hydra.core.TypeLambda;
import hydra.core.WrappedTerm;
import hydra.util.Comparison;
import hydra.util.Either;
import hydra.util.Maybe;
import hydra.util.Tuple;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static hydra.dsl.Core.name;
import static hydra.dsl.Literals.uint64;

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
     * @param ann the annotation map
     * @param base the base term to annotate
     * @return the annotated term
     */
    static Term annot(Map<Name, Term> ann, Term base) {
        return new Term.Annotated(new AnnotatedTerm(base, ann));
    }

    /**
     * Attach an annotation with a single key/value pair.
     * @param key the annotation key
     * @param value the annotation value
     * @param base the base term to annotate
     * @return the annotated term
     */
    static Term annot(Name key, Term value, Term base) {
        Map<Name, Term> mp = new HashMap<>();
        mp.put(key, value);
        return annot(mp, base);
    }

    /**
     * Attach an annotation with "description" key.
     * @param description the description text
     * @param base the base term to annotate
     * @return the annotated term
     */
    static Term annot(String description, Term base) {
        return annot(name("description"), string(description), base);
    }

    /**
     * Attach an annotation to a term (alternative argument order).
     * @param base the base term to annotate
     * @param ann the annotation map
     * @return the annotated term
     */
    static Term annotated(Term base, Map<Name, Term> ann) {
        return annot(ann, base);
    }

    // ===== Application =====

    /**
     * Apply a function term to an argument.
     * Example: apply(var("capitalize"), string("arthur"))
     * @param func the function term
     * @param arg the argument term
     * @return the application term
     */
    static Term apply(Term func, Term arg) {
        return new Term.Application(new Application(func, arg));
    }

    /**
     * Apply a function to multiple arguments.
     * Example: apply(var("add"), int32(1), int32(2))
     * @param func the function term
     * @param args the argument terms
     * @return the application term
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
     * @param func the function term
     * @param args the list of argument terms
     * @return the application term
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
     * @param func the function term
     * @param args the argument terms
     * @return the application term
     */
    static Term app(Term func, Term... args) {
        return apply(func, args);
    }

    /**
     * Apply a variable to arguments.
     * @param func the function variable name
     * @param args the argument terms
     * @return the application term
     */
    static Term app(String func, Term... args) {
        return apply(var(func), args);
    }

    // ===== Literal terms (delegating to Literals.java for Java-specific mappings) =====

    /**
     * Construct a term from a literal value.
     * @param value the literal value
     * @return the literal term
     */
    static Term literal(Literal value) {
        return new Term.Literal(value);
    }

    /**
     * Create a binary data literal.
     * Example: binary("\\x48\\x65\\x00\\xff")
     * @param value the binary data string
     * @return the binary literal term
     */
    static Term binary(String value) {
        return literal(Literals.binary(value));
    }

    /**
     * Create a boolean literal.
     * @param value the boolean value
     * @return the boolean literal term
     */
    static Term boolean_(boolean value) {
        return literal(Literals.boolean_(value));
    }

    /**
     * Boolean false literal.
     * @return the false literal term
     */
    static Term false_() {
        return boolean_(false);
    }

    /**
     * Boolean true literal.
     * @return the true literal term
     */
    static Term true_() {
        return boolean_(true);
    }

    /**
     * Create a string literal.
     * @param value the string value
     * @return the string literal term
     */
    static Term string(String value) {
        return literal(Literals.string(value));
    }

    // ===== Integer terms (Java-specific primitive mappings preserved) =====

    /**
     * Create an integer literal with specified bit width.
     * @param value the integer value
     * @return the integer literal term
     */
    static Term integer(IntegerValue value) {
        return literal(Literals.integer(value));
    }

    /**
     * Create an int8 literal (Java byte → Hydra int8).
     * @param value the byte value
     * @return the int8 literal term
     */
    static Term int8(byte value) {
        return literal(Literals.int8(value));
    }

    /**
     * Create an int16 literal (Java short → Hydra int16).
     * @param value the short value
     * @return the int16 literal term
     */
    static Term int16(short value) {
        return literal(Literals.int16(value));
    }

    /**
     * Create an int32 literal (Java int → Hydra int32).
     * @param value the int value
     * @return the int32 literal term
     */
    static Term int32(int value) {
        return literal(Literals.int32(value));
    }

    /**
     * Create an int64 literal (Java long → Hydra int64).
     * @param value the long value
     * @return the int64 literal term
     */
    static Term int64(long value) {
        return literal(Literals.int64(value));
    }

    /**
     * Create a bigint literal (Java BigInteger → Hydra bigint).
     * @param value the BigInteger value
     * @return the bigint literal term
     */
    static Term bigint(BigInteger value) {
        return literal(Literals.bigint(value));
    }

    /**
     * Create a uint8 literal (Java char → Hydra uint8).
     * @param value the short value
     * @return the uint8 literal term
     */
    static Term uint8(short value) {
        return literal(Literals.uint8(value));
    }

    /**
     * Create a uint16 literal (Java char → Hydra uint16).
     * @param value the char value
     * @return the uint16 literal term
     */
    static Term uint16(char value) {
        return literal(Literals.uint16(value));
    }

    /**
     * Create a uint32 literal (Java long → Hydra uint32).
     * @param value the long value
     * @return the uint32 literal term
     */
    static Term uint32(long value) {
        return literal(Literals.uint32(value));
    }

    /**
     * Create a uint64 literal (Java BigInteger → Hydra uint64).
     * @param value the BigInteger value
     * @return the uint64 literal term
     */
    static Term uint64(BigInteger value) {
        return literal(Literals.uint64(value));
    }

    // ===== Floating point terms (Java-specific mappings preserved) =====

    /**
     * Create a floating-point literal with specified precision.
     * @param value the float value
     * @return the float literal term
     */
    static Term float_(FloatValue value) {
        return literal(Literals.float_(value));
    }

    /**
     * Create a float32 literal (Java float → Hydra float32).
     * @param value the float value
     * @return the float32 literal term
     */
    static Term float32(float value) {
        return literal(Literals.float32(value));
    }

    /**
     * Create a float64 literal (Java double → Hydra float64).
     * @param value the double value
     * @return the float64 literal term
     */
    static Term float64(double value) {
        return literal(Literals.float64(value));
    }

    /**
     * Create a bigfloat literal (Java double → Hydra bigfloat).
     * @param value the BigDecimal value
     * @return the bigfloat literal term
     */
    static Term bigfloat(BigDecimal value) {
        return literal(Literals.bigfloat(value));
    }

    // ===== Character and comparison =====

    /**
     * Create a character term as an int32 (ord value).
     * @param value the char value
     * @return the character term
     */
    static Term char_(char value) {
        return int32((int) value);
    }

    /**
     * Create a comparison term.
     * @param comp the comparison value
     * @return the comparison term
     */
    static Term comparison(Comparison comp) {
        return comp.accept(new Comparison.Visitor<Term>() {
            @Override
            public Term visit(Comparison.EqualTo instance) {
                return injectUnit(name("hydra.mantle.Comparison"), name("equalTo"));
            }

            @Override
            public Term visit(Comparison.LessThan instance) {
                return injectUnit(name("hydra.mantle.Comparison"), name("lessThan"));
            }

            @Override
            public Term visit(Comparison.GreaterThan instance) {
                return injectUnit(name("hydra.mantle.Comparison"), name("greaterThan"));
            }
        });
    }

    // ===== Function terms =====

    /**
     * Construct a function term.
     * @param func the function
     * @return the function term
     */
    static Term function(Function func) {
        return new Term.Function(func);
    }

    /**
     * Create a lambda function with one parameter.
     * Example: lambda("x", apply(var("x"), int32(1)))
     * @param param the parameter name
     * @param body the function body
     * @return the lambda term
     */
    static Term lambda(String param, Term body) {
        return function(new Function.Lambda(new Lambda(name(param), Maybe.nothing(), body)));
    }

    /**
     * Create a lambda with two parameters.
     * @param param1 the first parameter name
     * @param param2 the second parameter name
     * @param body the function body
     * @return the lambda term
     */
    static Term lambda(String param1, String param2, Term body) {
        return lambda(param1, lambda(param2, body));
    }

    /**
     * Create a lambda with three parameters.
     * @param param1 the first parameter name
     * @param param2 the second parameter name
     * @param param3 the third parameter name
     * @param body the function body
     * @return the lambda term
     */
    static Term lambda(String param1, String param2, String param3, Term body) {
        return lambda(param1, lambda(param2, lambda(param3, body)));
    }

    /**
     * Create a multi-parameter lambda function (curried).
     * Example: lambdas(Arrays.asList("x", "y"), apply(var("add"), var("x"), var("y")))
     * @param params the list of parameter names
     * @param body the function body
     * @return the lambda term
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
     * @param params the array of parameter names
     * @param body the function body
     * @return the lambda term
     */
    static Term lambdas(String[] params, Term body) {
        return lambdas(Arrays.asList(params), body);
    }

    /**
     * Create a lambda function with a given domain type.
     * Example: lambdaTyped("x", Types.int32(), list(var("x")))
     * @param param the parameter name
     * @param dom the domain type
     * @param body the function body
     * @return the lambda term
     */
    static Term lambdaTyped(String param, Type dom, Term body) {
        return function(new Function.Lambda(new Lambda(name(param), Maybe.just(dom), body)));
    }

    /**
     * Identity function.
     * @return the identity function term
     */
    static Term identity() {
        return lambda("x_", var("x_"));
    }

    /**
     * Create a constant function that always returns the same value.
     * @param value the constant value
     * @return the constant function term
     */
    static Term constant(Term value) {
        return lambda(IGNORED_VARIABLE, value);
    }

    /**
     * Compose two functions (apply g then f).
     * Example: compose(var("stringLength"), var("toString"))
     * @param f the first function
     * @param g the second function
     * @return the composed function term
     */
    static Term compose(Term f, Term g) {
        return lambda("arg_", apply(f, apply(g, var("arg_"))));
    }

    /**
     * Create a primitive function.
     * Example: primitive(name("hydra.lib.strings.length"))
     * @param primName the primitive function name
     * @return the primitive function term
     */
    static Term primitive(Name primName) {
        return function(new Function.Primitive(primName));
    }

    /**
     * Create a primitive function.
     * @param primName the primitive function name
     * @return the primitive function term
     */
    static Term primitive(String primName) {
        return primitive(name(primName));
    }

    // ===== Elimination terms =====

    /**
     * Construct an elimination term.
     * @param elim the elimination
     * @return the elimination term
     */
    static Term elimination(Elimination elim) {
        return function(new Function.Elimination(elim));
    }

    /**
     * Create a projection term for accessing record fields.
     * Example: project(name("Person"), name("firstName"))
     * @param recordName the record type name
     * @param fieldName the field name
     * @return the projection term
     */
    static Term project(Name recordName, Name fieldName) {
        return elimination(new Elimination.Record(new Projection(recordName, fieldName)));
    }

    /**
     * Create a projection term.
     * @param recordName the record type name
     * @param fieldName the field name
     * @return the projection term
     */
    static Term project(String recordName, String fieldName) {
        return project(name(recordName), name(fieldName));
    }

    /**
     * Project a field from a record (with Name recordName and String fieldName).
     * @param recordName the record type name
     * @param fieldName the field name
     * @return the projection term
     */
    static Term project(Name recordName, String fieldName) {
        return project(recordName, name(fieldName));
    }

    /**
     * Create an unwrap function for a wrapped type.
     * Example: unwrap(name("Email"))
     * @param wrapName the wrapped type name
     * @return the unwrap function term
     */
    static Term unwrap(Name wrapName) {
        return elimination(new Elimination.Wrap(wrapName));
    }

    /**
     * Create an unwrap function.
     * @param wrapName the wrapped type name
     * @return the unwrap function term
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
     * First element projection function for pairs.
     * @return the first element projection term
     */
    static Term first() {
        return primitive(name("hydra.lib.pairs.first"));
    }

    /**
     * Second element projection function for pairs.
     * @return the second element projection term
     */
    static Term second() {
        return primitive(name("hydra.lib.pairs.second"));
    }

    /**
     * Create a pattern match on a union type.
     * Example: match(name("Result"), Opt.of(string("unknown")),
     *               field("success", lambda("s", apply(var("processSuccess"), var("s")))),
     *               field("error", lambda("e", apply(var("handleError"), var("e")))))
     * @param typeName the union type name
     * @param defaultCase the optional default case
     * @param fields the case fields
     * @return the match term
     */
    static Term match(Name typeName, Maybe<Term> defaultCase, Field... fields) {
        return elimination(new Elimination.Union(new CaseStatement(typeName, defaultCase, Arrays.asList(fields))));
    }

    /**
     * Create a pattern match on a union type.
     * @param typeName the union type name
     * @param defaultCase the optional default case
     * @param fields the case fields
     * @return the match term
     */
    static Term match(Name typeName, Maybe<Term> defaultCase, List<Field> fields) {
        return elimination(new Elimination.Union(new CaseStatement(typeName, defaultCase, fields)));
    }

    /**
     * Create a pattern match on a union type (with string type name).
     * @param typeName the union type name
     * @param defaultCase the optional default case
     * @param fields the case fields
     * @return the match term
     */
    static Term match(String typeName, Maybe<Term> defaultCase, Field... fields) {
        return match(name(typeName), defaultCase, fields);
    }

    // ===== Let bindings =====

    /**
     * Create a let term with a single binding.
     * Example: let_("x", int32(1), var("x"))
     * @param varName the variable name
     * @param defined the term to bind
     * @param body the body term
     * @return the let term
     */
    static Term let_(String varName, Term defined, Term body) {
        Binding binding = new Binding(name(varName), defined, Maybe.nothing());
        return new Term.Let(new Let(Collections.singletonList(binding), body));
    }

    /**
     * Create a let term with any number of bindings.
     * Example: lets(Arrays.asList(field("x", int32(1)), field("y", int32(2))), pair(var("x"), var("y")))
     * @param bindings the list of field bindings
     * @param body the body term
     * @return the let term
     */
    static Term lets(List<Field> bindings, Term body) {
        List<Binding> letBindings = new ArrayList<>();
        for (Field f : bindings) {
            letBindings.add(new Binding(f.name, f.term, Maybe.nothing()));
        }
        return new Term.Let(new Let(letBindings, body));
    }

    /**
     * Create a let term with typed bindings.
     * @param bindings the list of typed bindings
     * @param body the body term
     * @return the let term
     */
    static Term letsTyped(List<Binding> bindings, Term body) {
        return new Term.Let(new Let(bindings, body));
    }

    // ===== Field construction =====

    /**
     * Create a field with the given name and value.
     * Example: field("age", int32(30))
     * @param name the field name
     * @param term the field value
     * @return the field
     */
    static Field field(String name, Term term) {
        return new Field(name(name), term);
    }

    /**
     * Create a field.
     * @param name the field name
     * @param term the field value
     * @return the field
     */
    static Field field(Name name, Term term) {
        return new Field(name, term);
    }

    /**
     * Create a field (with variable term).
     * @param name the field name
     * @param varName the variable name
     * @return the field
     */
    static Field field(String name, String varName) {
        return field(name, var(varName));
    }

    // ===== Collection terms =====

    /**
     * Create a list of terms.
     * Example: list(int32(1), int32(2), int32(3))
     * @param elements the list elements
     * @return the list term
     */
    static Term list(Term... elements) {
        return new Term.List(Arrays.asList(elements));
    }

    /**
     * Create a list of terms.
     * @param elements the list elements
     * @return the list term
     */
    static Term list(List<Term> elements) {
        return new Term.List(elements);
    }

    /**
     * Create a list of strings.
     * @param elements the string elements
     * @return the list term
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
     * @param value the map value
     * @return the map term
     */
    static Term map(Map<Term, Term> value) {
        return new Term.Map(value);
    }

    /**
     * Create a set of terms.
     * Example: set(Set.of(string("a"), string("b"), string("c")))
     * @param value the set value
     * @return the set term
     */
    static Term set(Set<Term> value) {
        return new Term.Set(value);
    }

    // ===== Optional terms =====

    /**
     * Create an optional (nullable) term.
     * @param maybeTerm the optional term
     * @return the optional term
     */
    static Term optional(Maybe<Term> maybeTerm) {
        return new Term.Maybe(maybeTerm);
    }

    /**
     * Create a 'Nothing' optional value.
     * @return the nothing term
     */
    static Term nothing() {
        return optional(Maybe.nothing());
    }

    /**
     * Create a 'Just' optional value.
     * Example: just(string("found"))
     * @param elem the element term
     * @return the just term
     */
    static Term just(Term elem) {
        return optional(Maybe.just(elem));
    }

    /**
     * Create a 'Just' optional value (with variable).
     * @param varName the variable name
     * @return the just term
     */
    static Term just(String varName) {
        return just(var(varName));
    }

    // ===== Pair terms =====

    /**
     * Create a pair.
     * Example: pair(string("name"), int32(42))
     * @param a the first element
     * @param b the second element
     * @return the pair term
     */
    static Term pair(Term a, Term b) {
        return new Term.Pair(new Tuple.Tuple2<>(a, b));
    }

    /**
     * Create a 2-tuple (alias for pair).
     * @param a the first element
     * @param b the second element
     * @return the tuple term
     */
    static Term tuple2(Term a, Term b) {
        return pair(a, b);
    }

    /**
     * Create a tuple using nested pairs.
     * Example: tuple(a, b, c) creates pair(a, pair(b, c))
     * @param elements the tuple elements
     * @return the tuple term as nested pairs
     */
    static Term tuple(Term... elements) {
        return tuple(Arrays.asList(elements));
    }

    /**
     * Create a tuple using nested pairs.
     * @param elements the tuple elements
     * @return the tuple term as nested pairs
     */
    static Term tuple(List<Term> elements) {
        if (elements.isEmpty()) {
            return unit();
        }
        if (elements.size() == 1) {
            return elements.get(0);
        }
        if (elements.size() == 2) {
            return pair(elements.get(0), elements.get(1));
        }
        return pair(elements.get(0), tuple(elements.subList(1, elements.size())));
    }

    /**
     * Create a product term using nested pairs (alias for tuple).
     * @param elements the product elements
     * @return the product term as nested pairs
     */
    static Term product(Term... elements) {
        return tuple(elements);
    }

    /**
     * Create a product term using nested pairs (alias for tuple).
     * @param elements the product elements
     * @return the product term as nested pairs
     */
    static Term product(List<Term> elements) {
        return tuple(elements);
    }

    /**
     * Create a triple using nested pairs.
     * @param a the first element
     * @param b the second element
     * @param c the third element
     * @return the triple term as pair(a, pair(b, c))
     */
    static Term triple(Term a, Term b, Term c) {
        return pair(a, pair(b, c));
    }

    /**
     * Create a 3-tuple (alias for triple).
     * @param a the first element
     * @param b the second element
     * @param c the third element
     * @return the tuple term
     */
    static Term tuple3(Term a, Term b, Term c) {
        return triple(a, b, c);
    }

    /**
     * Create a 4-tuple using nested pairs.
     * @param a the first element
     * @param b the second element
     * @param c the third element
     * @param d the fourth element
     * @return the 4-tuple term as nested pairs
     */
    static Term tuple4(Term a, Term b, Term c, Term d) {
        return pair(a, pair(b, pair(c, d)));
    }

    /**
     * Create a 5-tuple using nested pairs.
     * @param a the first element
     * @param b the second element
     * @param c the third element
     * @param d the fourth element
     * @param e the fifth element
     * @return the 5-tuple term as nested pairs
     */
    static Term tuple5(Term a, Term b, Term c, Term d, Term e) {
        return pair(a, pair(b, pair(c, pair(d, e))));
    }

    // ===== Either terms =====

    /**
     * Create a 'Left' either value.
     * Example: left(string("error"))
     * @param term the left term
     * @return the left term
     */
    static Term left(Term term) {
        return new Term.Either(Either.left(term));
    }

    /**
     * Create a 'Right' either value.
     * Example: right(int32(42))
     * @param term the right term
     * @return the right term
     */
    static Term right(Term term) {
        return new Term.Either(Either.right(term));
    }

    // ===== Record terms =====

    /**
     * Create a record with named fields.
     * Example: record(name("Person"),
     *            field("name", string("John")),
     *            field("age", int32(30)))
     * @param typeName the record type name
     * @param fields the record fields
     * @return the record term
     */
    static Term record(Name typeName, Field... fields) {
        return new Term.Record(new Record(typeName, Arrays.asList(fields)));
    }

    /**
     * Create a record term.
     * @param typeName the record type name
     * @param fields the record fields
     * @return the record term
     */
    static Term record(Name typeName, List<Field> fields) {
        return new Term.Record(new Record(typeName, fields));
    }

    /**
     * Create a record term (with string type name).
     * @param typeName the record type name
     * @param fields the record fields
     * @return the record term
     */
    static Term record(String typeName, Field... fields) {
        return record(name(typeName), fields);
    }

    /**
     * Unit value.
     * @return the unit term
     */
    static Term unit() {
        return new Term.Unit(true);
    }

    // ===== Union terms =====

    /**
     * Create a union value by injecting a value into a specific variant.
     * Example: inject(name("Result"), field("success", int32(42)))
     * @param typeName the union type name
     * @param field the injected field
     * @return the injection term
     */
    static Term inject(Name typeName, Field field) {
        return new Term.Union(new Injection(typeName, field));
    }

    /**
     * Create an injection term.
     * @param typeName the union type name
     * @param field the injected field
     * @return the injection term
     */
    static Term inject(String typeName, Field field) {
        return inject(name(typeName), field);
    }

    /**
     * Create an injection with a field name and value.
     * Example: inject(name("Result"), name("success"), string("ok"))
     * @param typeName the union type name
     * @param fieldName the field name
     * @param term the field value
     * @return the injection term
     */
    static Term inject(Name typeName, Name fieldName, Term term) {
        return inject(typeName, new Field(fieldName, term));
    }

    /**
     * Create an injection with a field name and value.
     * @param typeName the union type name
     * @param fieldName the field name
     * @param term the field value
     * @return the injection term
     */
    static Term inject(String typeName, String fieldName, Term term) {
        return inject(name(typeName), name(fieldName), term);
    }

    /**
     * Create a unit-valued injection, similar to an enum value.
     * Example: injectUnit(name("Result"), name("success"))
     * @param typeName the union type name
     * @param fieldName the field name
     * @return the injection term
     */
    static Term injectUnit(Name typeName, Name fieldName) {
        return inject(typeName, new Field(fieldName, unit()));
    }

    /**
     * Create a unit-valued injection.
     * @param typeName the union type name
     * @param fieldName the field name
     * @return the injection term
     */
    static Term injectUnit(String typeName, String fieldName) {
        return injectUnit(name(typeName), name(fieldName));
    }

    // ===== Wrapped terms =====

    /**
     * Create a wrapped term.
     * Example: wrap(name("Email"), string("user@example.com"))
     * @param wrapName the wrapped type name
     * @param term the wrapped term
     * @return the wrap term
     */
    static Term wrap(Name wrapName, Term term) {
        return new Term.Wrap(new WrappedTerm(wrapName, term));
    }

    /**
     * Create a wrapped term.
     * @param wrapName the wrapped type name
     * @param term the wrapped term
     * @return the wrap term
     */
    static Term wrap(String wrapName, Term term) {
        return wrap(name(wrapName), term);
    }

    // ===== Variables =====

    /**
     * Create a variable reference.
     * Example: var("x")
     * @param name the variable name
     * @return the variable term
     */
    static Term var(String name) {
        return new Term.Variable(name(name));
    }

    /**
     * Create a variable reference.
     * @param name the variable name
     * @return the variable term
     */
    static Term variable(Name name) {
        return new Term.Variable(name);
    }

    /**
     * Create a variable reference.
     * @param name the variable name
     * @return the variable term
     */
    static Term variable(String name) {
        return var(name);
    }

    // ===== Type abstraction and application =====

    /**
     * Create a type lambda (type abstraction).
     * Example: tylam("a", lambda("x", var("x")))
     * @param var the type variable name
     * @param body the body term
     * @return the type lambda term
     */
    static Term tylam(String var, Term body) {
        return new Term.TypeLambda(new TypeLambda(name(var), body));
    }

    /**
     * Create a type lambda with multiple variables.
     * @param vars the list of type variable names
     * @param body the body term
     * @return the type lambda term
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
     * @param vars the array of type variable names
     * @param body the body term
     * @return the type lambda term
     */
    static Term tylams(String[] vars, Term body) {
        return tylams(Arrays.asList(vars), body);
    }

    /**
     * Create a type abstraction (universal quantification).
     * @param vars the list of type variable names
     * @param body the body term
     * @return the type lambda term
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
     * @param term the polymorphic term
     * @param typ the type argument
     * @return the type application term
     */
    static Term tyapp(Term term, Type typ) {
        return new Term.TypeApplication(new TypeApplicationTerm(term, typ));
    }

    /**
     * Apply type arguments to a polymorphic term.
     * Example: tyapps(var("map"), Arrays.asList(Types.int32(), Types.string()))
     * @param term the polymorphic term
     * @param types the list of type arguments
     * @return the type application term
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
     * @param term the polymorphic term
     * @param types the array of type arguments
     * @return the type application term
     */
    static Term tyapps(Term term, Type... types) {
        return tyapps(term, Arrays.asList(types));
    }

    /**
     * Apply type arguments to a polymorphic term (alias).
     * @param term the polymorphic term
     * @param types the list of type arguments
     * @return the type application term
     */
    static Term typeApplication(Term term, List<Type> types) {
        return tyapps(term, types);
    }

    // ===== Flow monad helpers =====

    /**
     * Construct a flow state term (used with the Flow monad).
     * @param value the value term
     * @param state the state term
     * @param trace the trace term
     * @return the flow state term
     */
    static Term flowState(Term value, Term state, Term trace) {
        return record(FlowState.TYPE_NAME,
                field("value", value),
                field("state", state),
                field("trace", trace));
    }

    /**
     * Construct a value projection (used with the Flow monad).
     * @return the value projection term
     */
    static Term flowStateValue() {
        return project(FlowState.TYPE_NAME, name("value"));
    }

    /**
     * Construct a state projection (used with the Flow monad).
     * @return the state projection term
     */
    static Term flowStateState() {
        return project(FlowState.TYPE_NAME, name("state"));
    }

    /**
     * Construct a trace projection (used with the Flow monad).
     * @return the trace projection term
     */
    static Term flowStateTrace() {
        return project(FlowState.TYPE_NAME, name("trace"));
    }
}
