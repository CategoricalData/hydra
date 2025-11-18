package hydra.dsl;

import hydra.core.AnnotatedType;
import hydra.core.ApplicationType;
import hydra.core.FieldType;
import hydra.core.FloatType;
import hydra.core.ForallType;
import hydra.core.FunctionType;
import hydra.core.IntegerType;
import hydra.core.LiteralType;
import hydra.core.MapType;
import hydra.core.Name;
import hydra.core.RowType;
import hydra.core.Term;
import hydra.core.Type;
import hydra.core.TypeScheme;
import hydra.core.WrappedType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static hydra.dsl.Core.name;

/**
 * A domain-specific language for constructing Hydra types in Java.
 * Provides parity with Hydra.Dsl.Types (Haskell) and hydra.dsl.types (Python).
 */
public interface Types {
    Name PLACEHOLDER_NAME = name("_Placeholder");

    // ===== Type annotations =====

    /**
     * Attach an annotation to a type.
     * Example: annot(Map.of(name("min"), int32(0)), int32())
     * @param ann the annotation map
     * @param base the base type
     * @return the annotated type
     */
    static Type annot(Map<Name, Term> ann, Type base) {
        return new Type.Annotated(new AnnotatedType(base, ann));
    }

    /**
     * Attach an annotation with a single key/value pair to a type.
     * @param key the annotation key
     * @param value the annotation value
     * @param base the base type
     * @return the annotated type
     */
    static Type annot(Name key, Term value, Type base) {
        Map<Name, Term> mp = new HashMap<>();
        mp.put(key, value);
        return annot(mp, base);
    }

    // ===== Type application =====

    /**
     * Apply a type to a type argument.
     * Example: apply(var("List"), int32())
     * @param lhs the type function
     * @param rhs the type argument
     * @return the application type
     */
    static Type apply(Type lhs, Type rhs) {
        return new Type.Application(new ApplicationType(lhs, rhs));
    }

    /**
     * Apply a type to multiple type arguments.
     * Example: apply(var("Either"), string(), int32())
     * @param lhs the type function
     * @param rhs the type arguments
     * @return the application type
     */
    static Type apply(Type lhs, Type... rhs) {
        Type cur = lhs;
        for (Type r : rhs) {
            cur = apply(cur, r);
        }
        return cur;
    }

    /**
     * Apply a type to multiple type arguments.
     * Example: applys(var("f"), Arrays.asList(int32(), string()))
     * @param t the type function
     * @param ts the list of type arguments
     * @return the application type
     */
    static Type applys(Type t, List<Type> ts) {
        Type cur = t;
        for (Type arg : ts) {
            cur = apply(cur, arg);
        }
        return cur;
    }

    /**
     * Apply a type to multiple type arguments (first element is the function).
     * Example: applyMany(Arrays.asList(var("Either"), string(), int32()))
     * @param ts the list of types (first is the function, rest are arguments)
     * @return the application type
     */
    static Type applyMany(List<Type> ts) {
        if (ts.isEmpty()) {
            throw new IllegalArgumentException("applyMany requires at least one type");
        }
        Type result = ts.get(0);
        for (int i = 1; i < ts.size(); i++) {
            result = apply(result, ts.get(i));
        }
        return result;
    }

    // ===== Literal types =====

    /**
     * Construct a literal type.
     * @param ltype the literal type
     * @return the type
     */
    static Type literal(LiteralType ltype) {
        return new Type.Literal(ltype);
    }

    /**
     * Binary data type.
     * @return the binary type
     */
    static Type binary() {
        return literal(LiteralTypes.binary());
    }

    /**
     * Boolean type.
     * @return the boolean type
     */
    static Type boolean_() {
        return literal(LiteralTypes.boolean_());
    }

    /**
     * String type.
     * @return the string type
     */
    static Type string() {
        return literal(LiteralTypes.string());
    }

    // ===== Integer types =====

    /**
     * Construct an integer type with the specified bit width.
     * @param itype the integer type
     * @return the type
     */
    static Type integer(IntegerType itype) {
        return literal(LiteralTypes.integer(itype));
    }

    /**
     * 8-bit signed integer type.
     * @return the int8 type
     */
    static Type int8() {
        return literal(LiteralTypes.int8());
    }

    /**
     * 16-bit signed integer type.
     * @return the int16 type
     */
    static Type int16() {
        return literal(LiteralTypes.int16());
    }

    /**
     * 32-bit signed integer type.
     * @return the int32 type
     */
    static Type int32() {
        return literal(LiteralTypes.int32());
    }

    /**
     * 64-bit signed integer type.
     * @return the int64 type
     */
    static Type int64() {
        return literal(LiteralTypes.int64());
    }

    /**
     * Arbitrary-precision integer type.
     * @return the bigint type
     */
    static Type bigint() {
        return literal(LiteralTypes.bigint());
    }

    /**
     * 8-bit unsigned integer type.
     * @return the uint8 type
     */
    static Type uint8() {
        return literal(LiteralTypes.uint8());
    }

    /**
     * 16-bit unsigned integer type.
     * @return the uint16 type
     */
    static Type uint16() {
        return literal(LiteralTypes.uint16());
    }

    /**
     * 32-bit unsigned integer type.
     * @return the uint32 type
     */
    static Type uint32() {
        return literal(LiteralTypes.uint32());
    }

    /**
     * 64-bit unsigned integer type.
     * @return the uint64 type
     */
    static Type uint64() {
        return literal(LiteralTypes.uint64());
    }

    // ===== Floating point types =====

    /**
     * Construct a floating point type with the specified precision.
     * @param ftype the float type
     * @return the type
     */
    static Type float_(FloatType ftype) {
        return literal(LiteralTypes.float_(ftype));
    }

    /**
     * 32-bit floating point type.
     * @return the float32 type
     */
    static Type float32() {
        return literal(LiteralTypes.float32());
    }

    /**
     * 64-bit floating point type.
     * @return the float64 type
     */
    static Type float64() {
        return literal(LiteralTypes.float64());
    }

    /**
     * Arbitrary-precision floating point type.
     * @return the bigfloat type
     */
    static Type bigfloat() {
        return literal(LiteralTypes.bigfloat());
    }

    /**
     * Non-negative 32-bit integer type.
     * Currently an alias for int32; intended for semantic annotation.
     * @return the non-negative int32 type
     */
    static Type nonNegativeInt32() {
        return int32();
    }

    // ===== Function types =====

    /**
     * Create a function type.
     * Example: function(int32(), string())
     * @param dom the domain type
     * @param cod the codomain type
     * @return the function type
     */
    static Type function(Type dom, Type cod) {
        return new Type.Function(new FunctionType(dom, cod));
    }

    /**
     * Create a function type (with string variable name for domain).
     * @param dom the domain type variable name
     * @param cod the codomain type
     * @return the function type
     */
    static Type function(String dom, Type cod) {
        return function(var(dom), cod);
    }

    /**
     * Create a function type (with string variable name for codomain).
     * @param dom the domain type
     * @param cod the codomain type variable name
     * @return the function type
     */
    static Type function(Type dom, String cod) {
        return function(dom, var(cod));
    }

    /**
     * Create a function type (with string variable names for both).
     * @param dom the domain type variable name
     * @param cod the codomain type variable name
     * @return the function type
     */
    static Type function(String dom, String cod) {
        return function(var(dom), var(cod));
    }

    /**
     * Create a function type with multiple arguments.
     * Example: function(int32(), string(), boolean()) creates {@literal int32 -> (string -> boolean)}
     * @param dom the domain type
     * @param cod the codomain type
     * @param more additional argument types
     * @return the function type
     */
    static Type function(Type dom, Type cod, Type... more) {
        Type c;
        if (more.length == 0) {
            c = cod;
        } else {
            c = more[more.length - 1];
            for (int i = more.length - 2; i >= 0; i--) {
                c = function(more[i], c);
            }
            c = function(cod, c);
        }
        return function(dom, c);
    }

    /**
     * Create an n-ary function type.
     * Example: functionMany(Arrays.asList(int32(), string(), boolean())) creates {@literal int32 -> string -> boolean}
     * @param ts the list of types (last is the codomain)
     * @return the function type
     */
    static Type functionMany(List<Type> ts) {
        if (ts.size() < 2) {
            throw new IllegalArgumentException("functionMany requires at least 2 types");
        }
        List<Type> reversed = new ArrayList<>(ts);
        Collections.reverse(reversed);
        Type result = reversed.get(0);
        for (int i = 1; i < reversed.size(); i++) {
            result = function(reversed.get(i), result);
        }
        return result;
    }

    // ===== Universal quantification (forall) =====

    /**
     * Create a universally quantified type (polymorphic type) with a single variable.
     * Example: forall("a", function(var("a"), var("a")))
     * This creates the polymorphic identity function type: {@literal ∀a. a -> a}
     * @param v the type variable name
     * @param body the body type
     * @return the forall type
     */
    static Type forall(String v, Type body) {
        return new Type.Forall(new ForallType(name(v), body));
    }

    /**
     * Create a universally quantified type with two variables.
     * @param v1 the first type variable name
     * @param v2 the second type variable name
     * @param body the body type
     * @return the forall type
     */
    static Type forall(String v1, String v2, Type body) {
        return forall(v1, forall(v2, body));
    }

    /**
     * Create a universally quantified type with three variables.
     * @param v1 the first type variable name
     * @param v2 the second type variable name
     * @param v3 the third type variable name
     * @param body the body type
     * @return the forall type
     */
    static Type forall(String v1, String v2, String v3, Type body) {
        return forall(v1, forall(v2, forall(v3, body)));
    }

    /**
     * Universal quantification with multiple variables.
     * Example: foralls(Arrays.asList("a", "b"), function(var("a"), var("b")))
     * @param vs the list of type variable names
     * @param body the body type
     * @return the forall type
     */
    static Type foralls(List<String> vs, Type body) {
        Type result = body;
        for (int i = vs.size() - 1; i >= 0; i--) {
            result = forall(vs.get(i), result);
        }
        return result;
    }

    // ===== Type schemes =====

    /**
     * Create a monomorphic type scheme (no type variables).
     * @param body the body type
     * @return the type scheme
     */
    static TypeScheme mono(Type body) {
        return new TypeScheme(Collections.emptyList(), body);
    }

    /**
     * Create a polymorphic type scheme with explicit type variables.
     * Example: poly(Arrays.asList("a", "b"), function(var("a"), var("b")))
     * @param vs the list of type variable names
     * @param body the body type
     * @return the type scheme
     */
    static TypeScheme poly(List<String> vs, Type body) {
        List<Name> names = new ArrayList<>();
        for (String v : vs) {
            names.add(name(v));
        }
        return new TypeScheme(names, body);
    }

    /**
     * Create a type scheme with one type variable.
     * @param var the type variable name
     * @param body the body type
     * @return the type scheme
     */
    static TypeScheme scheme(String var, Type body) {
        return new TypeScheme(Collections.singletonList(name(var)), body);
    }

    /**
     * Create a type scheme with two type variables.
     * @param var1 the first type variable name
     * @param var2 the second type variable name
     * @param body the body type
     * @return the type scheme
     */
    static TypeScheme scheme(String var1, String var2, Type body) {
        return new TypeScheme(Arrays.asList(name(var1), name(var2)), body);
    }

    /**
     * Create a type scheme with three type variables.
     * @param var1 the first type variable name
     * @param var2 the second type variable name
     * @param var3 the third type variable name
     * @param body the body type
     * @return the type scheme
     */
    static TypeScheme scheme(String var1, String var2, String var3, Type body) {
        return new TypeScheme(Arrays.asList(name(var1), name(var2), name(var3)), body);
    }

    /**
     * Create a type scheme with no type variables.
     * @param body the body type
     * @return the type scheme
     */
    static TypeScheme scheme(Type body) {
        return mono(body);
    }

    // ===== Collection types =====

    /**
     * List type.
     * Example: list(string())
     * @param elements the element type
     * @return the list type
     */
    static Type list(Type elements) {
        return new Type.List(elements);
    }

    /**
     * List type (with string variable name).
     * @param elements the element type variable name
     * @return the list type
     */
    static Type list(String elements) {
        return list(var(elements));
    }

    /**
     * Map/dictionary type with key and value types.
     * Example: map(string(), int32())
     * @param keys the key type
     * @param values the value type
     * @return the map type
     */
    static Type map(Type keys, Type values) {
        return new Type.Map(new MapType(keys, values));
    }

    /**
     * Map type (with string variable names).
     * @param keys the key type variable name
     * @param values the value type variable name
     * @return the map type
     */
    static Type map(String keys, String values) {
        return map(var(keys), var(values));
    }

    /**
     * Optional (nullable) type.
     * Example: optional(string())
     * @param elements the element type
     * @return the optional type
     */
    static Type optional(Type elements) {
        return new Type.Maybe(elements);
    }

    /**
     * Optional type (with string variable name).
     * @param elements the element type variable name
     * @return the optional type
     */
    static Type optional(String elements) {
        return optional(var(elements));
    }

    /**
     * Set type.
     * Example: set(string())
     * @param elements the element type
     * @return the set type
     */
    static Type set(Type elements) {
        return new Type.Set(elements);
    }

    /**
     * Set type (with string variable name).
     * @param elements the element type variable name
     * @return the set type
     */
    static Type set(String elements) {
        return set(var(elements));
    }

    /**
     * Flow type (monadic computation with state, trace, and error handling).
     * Example: {@literal flow(var("s"), var("x"))  // Flow<S, X>}
     * @param state the state type
     * @param value the value type
     * @return the flow type
     */
    static Type flow(Type state, Type value) {
        return apply(apply(new Type.Variable(new Name("hydra.compute.Flow")), state), value);
    }

    /**
     * Flow type (with string variable names).
     * Example: {@literal flow("s", "x")  // Flow<S, X>}
     * @param state the state type variable name
     * @param value the value type variable name
     * @return the flow type
     */
    static Type flow(String state, String value) {
        return flow(var(state), var(value));
    }

    /**
     * Flow type with Type state and String value variable.
     * @param state the state type variable name
     * @param value the value type
     * @return the flow type
     */
    static Type flow(String state, Type value) {
        return flow(var(state), value);
    }

    // ===== Product types =====

    /**
     * Create a pair type.
     * Example: pair(string(), int32())
     * @param first the first element type
     * @param second the second element type
     * @return the pair type
     */
    static Type pair(Type first, Type second) {
        return new Type.Product(Arrays.asList(first, second));
    }

    /**
     * Create a 2-tuple type (alias for pair).
     * @param a the first element type
     * @param b the second element type
     * @return the tuple type
     */
    static Type tuple2(Type a, Type b) {
        return new Type.Product(Arrays.asList(a, b));
    }

    /**
     * Create a product type (tuple) with multiple components.
     * Example: product(Arrays.asList(string(), int32(), boolean()))
     * @param types the list of element types
     * @return the product type
     */
    static Type product(List<Type> types) {
        return new Type.Product(types);
    }

    /**
     * Create a product type from varargs.
     * @param types the element types
     * @return the product type
     */
    static Type product(Type... types) {
        return new Type.Product(Arrays.asList(types));
    }

    // ===== Sum types =====

    /**
     * Create an either type (a choice between two types).
     * Example: either(string(), int32())
     * @param left the left type
     * @param right the right type
     * @return the either type
     */
    static Type either(Type left, Type right) {
        return new Type.Sum(Arrays.asList(left, right));
    }

    /**
     * Create a sum type (disjoint union) with multiple variants.
     * Example: sum(Arrays.asList(string(), int32(), boolean()))
     * @param types the list of variant types
     * @return the sum type
     */
    static Type sum(List<Type> types) {
        return new Type.Sum(types);
    }

    /**
     * Create a sum type from varargs.
     * @param types the variant types
     * @return the sum type
     */
    static Type sum(Type... types) {
        return new Type.Sum(Arrays.asList(types));
    }

    // ===== Record and union types =====

    /**
     * Create a field with the given name and type.
     * Example: field("age", int32())
     * @param name the field name
     * @param t the field type
     * @return the field type
     */
    static FieldType field(String name, Type t) {
        return new FieldType(name(name), t);
    }

    /**
     * Create a row type.
     * @param name the row type name
     * @param fields the list of fields
     * @return the row type
     */
    static RowType row(Name name, List<FieldType> fields) {
        return new RowType(name, fields);
    }

    /**
     * Create a row type from varargs.
     * @param name the row type name
     * @param fields the fields
     * @return the row type
     */
    static RowType row(Name name, FieldType... fields) {
        return new RowType(name, Arrays.asList(fields));
    }

    /**
     * Create a record type with the given fields and the default type name.
     * Example: record(field("name", string()), field("age", int32()))
     * @param fields the fields
     * @return the record type
     */
    static Type record(FieldType... fields) {
        return recordWithName(PLACEHOLDER_NAME, fields);
    }

    /**
     * Create a record type with the given fields.
     * @param fields the list of fields
     * @return the record type
     */
    static Type record(List<FieldType> fields) {
        return recordWithName(PLACEHOLDER_NAME, fields);
    }

    /**
     * Create a record type with the given fields and a provided type name.
     * Example: recordWithName(name("Person"), Arrays.asList(field("name", string()), field("age", int32())))
     * @param tname the type name
     * @param fields the list of fields
     * @return the record type
     */
    static Type recordWithName(Name tname, List<FieldType> fields) {
        return new Type.Record(row(tname, fields));
    }

    /**
     * Create a record type with the given fields and a provided type name.
     * @param tname the type name
     * @param fields the fields
     * @return the record type
     */
    static Type recordWithName(Name tname, FieldType... fields) {
        return new Type.Record(row(tname, fields));
    }

    /**
     * Create a record type with the given name and fields.
     * @param tname the type name
     * @param fields the fields
     * @return the record type
     */
    static Type recordWithName(String tname, FieldType... fields) {
        return recordWithName(name(tname), fields);
    }

    /**
     * Unit type (empty record type).
     * @return the unit type
     */
    static Type unit() {
        return new Type.Record(row(name("_Unit"), Collections.emptyList()));
    }

    /**
     * Create an enumerated type with the given variant names.
     * Example: enum_("red", "green", "blue")
     * @param names the variant names
     * @return the enum type
     */
    static Type enum_(String... names) {
        FieldType[] fields = new FieldType[names.length];
        for (int i = 0; i < names.length; i++) {
            fields[i] = field(names[i], unit());
        }
        return union(fields);
    }

    /**
     * Create a union type with the given variants and the default type name.
     * Example: union(field("success", int32()), field("failure", string()))
     * @param fields the variant fields
     * @return the union type
     */
    static Type union(FieldType... fields) {
        return new Type.Union(row(PLACEHOLDER_NAME, fields));
    }

    /**
     * Create a union type with the given variants.
     * @param fields the list of variant fields
     * @return the union type
     */
    static Type union(List<FieldType> fields) {
        return new Type.Union(row(PLACEHOLDER_NAME, fields));
    }

    /**
     * Create a union type with the given variants and a provided type name.
     * @param tname the type name
     * @param fields the variant fields
     * @return the union type
     */
    static Type union(Name tname, FieldType... fields) {
        return new Type.Union(row(tname, fields));
    }

    /**
     * Create a union type with the given name and variants.
     * @param tname the type name
     * @param fields the variant fields
     * @return the union type
     */
    static Type union(String tname, FieldType... fields) {
        return union(name(tname), fields);
    }

    // ===== Wrapped types =====

    /**
     * Create a wrapped type (newtype) with a provided base type and the default type name.
     * Example: wrap(string())
     * @param t the base type
     * @return the wrapped type
     */
    static Type wrap(Type t) {
        return wrapWithName(PLACEHOLDER_NAME, t);
    }

    /**
     * Create a wrapped type (newtype) with a provided base type and type name.
     * Example: wrapWithName(name("Email"), string())
     * @param tname the type name
     * @param t the base type
     * @return the wrapped type
     */
    static Type wrapWithName(Name tname, Type t) {
        return new Type.Wrap(new WrappedType(tname, t));
    }

    /**
     * Create a wrapped type with the given name.
     * @param tname the type name
     * @param t the base type
     * @return the wrapped type
     */
    static Type wrapWithName(String tname, Type t) {
        return wrapWithName(name(tname), t);
    }

    // ===== Type variables =====

    /**
     * Create a type variable with the given name.
     * Example: var("a")
     * @param name the variable name
     * @return the type variable
     */
    static Type var(String name) {
        return new Type.Variable(name(name));
    }

    /**
     * Create a type variable.
     * @param name the variable name
     * @return the type variable
     */
    static Type variable(Name name) {
        return new Type.Variable(name);
    }

    /**
     * Create a type variable.
     * @param name the variable name
     * @return the type variable
     */
    static Type variable(String name) {
        return var(name);
    }
}
