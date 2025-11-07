package hydra.dsl;

import hydra.core.*;
import hydra.util.Opt;

import java.util.*;

import static hydra.dsl.Core.name;
import static hydra.dsl.LiteralTypes.*;

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
     */
    static Type annot(Map<Name, Term> ann, Type base) {
        return new Type.Annotated(new AnnotatedType(base, ann));
    }

    /**
     * Attach an annotation with a single key/value pair to a type.
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
     */
    static Type apply(Type lhs, Type rhs) {
        return new Type.Application(new ApplicationType(lhs, rhs));
    }

    /**
     * Apply a type to multiple type arguments.
     * Example: apply(var("Either"), string(), int32())
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
     */
    static Type literal(LiteralType ltype) {
        return new Type.Literal(ltype);
    }

    /**
     * Binary data type.
     */
    static Type binary() {
        return literal(LiteralTypes.binary());
    }

    /**
     * Boolean type.
     */
    static Type boolean_() {
        return literal(LiteralTypes.boolean_());
    }

    /**
     * String type.
     */
    static Type string() {
        return literal(LiteralTypes.string());
    }

    // ===== Integer types =====

    /**
     * Construct an integer type with the specified bit width.
     */
    static Type integer(IntegerType itype) {
        return literal(LiteralTypes.integer(itype));
    }

    /**
     * 8-bit signed integer type.
     */
    static Type int8() {
        return literal(LiteralTypes.int8());
    }

    /**
     * 16-bit signed integer type.
     */
    static Type int16() {
        return literal(LiteralTypes.int16());
    }

    /**
     * 32-bit signed integer type.
     */
    static Type int32() {
        return literal(LiteralTypes.int32());
    }

    /**
     * 64-bit signed integer type.
     */
    static Type int64() {
        return literal(LiteralTypes.int64());
    }

    /**
     * Arbitrary-precision integer type.
     */
    static Type bigint() {
        return literal(LiteralTypes.bigint());
    }

    /**
     * 8-bit unsigned integer type.
     */
    static Type uint8() {
        return literal(LiteralTypes.uint8());
    }

    /**
     * 16-bit unsigned integer type.
     */
    static Type uint16() {
        return literal(LiteralTypes.uint16());
    }

    /**
     * 32-bit unsigned integer type.
     */
    static Type uint32() {
        return literal(LiteralTypes.uint32());
    }

    /**
     * 64-bit unsigned integer type.
     */
    static Type uint64() {
        return literal(LiteralTypes.uint64());
    }

    // ===== Floating point types =====

    /**
     * Construct a floating point type with the specified precision.
     */
    static Type float_(FloatType ftype) {
        return literal(LiteralTypes.float_(ftype));
    }

    /**
     * 32-bit floating point type.
     */
    static Type float32() {
        return literal(LiteralTypes.float32());
    }

    /**
     * 64-bit floating point type.
     */
    static Type float64() {
        return literal(LiteralTypes.float64());
    }

    /**
     * Arbitrary-precision floating point type.
     */
    static Type bigfloat() {
        return literal(LiteralTypes.bigfloat());
    }

    /**
     * Non-negative 32-bit integer type.
     * Currently an alias for int32; intended for semantic annotation.
     */
    static Type nonNegativeInt32() {
        return int32();
    }

    // ===== Function types =====

    /**
     * Create a function type.
     * Example: function(int32(), string())
     */
    static Type function(Type dom, Type cod) {
        return new Type.Function(new FunctionType(dom, cod));
    }

    /**
     * Create a function type (with string variable name for domain).
     */
    static Type function(String dom, Type cod) {
        return function(var(dom), cod);
    }

    /**
     * Create a function type (with string variable name for codomain).
     */
    static Type function(Type dom, String cod) {
        return function(dom, var(cod));
    }

    /**
     * Create a function type (with string variable names for both).
     */
    static Type function(String dom, String cod) {
        return function(var(dom), var(cod));
    }

    /**
     * Create a function type with multiple arguments.
     * Example: function(int32(), string(), boolean()) creates {@literal int32 -> (string -> boolean)}
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
     */
    static Type forall(String v, Type body) {
        return new Type.Forall(new ForallType(name(v), body));
    }

    /**
     * Create a universally quantified type with two variables.
     */
    static Type forall(String v1, String v2, Type body) {
        return forall(v1, forall(v2, body));
    }

    /**
     * Create a universally quantified type with three variables.
     */
    static Type forall(String v1, String v2, String v3, Type body) {
        return forall(v1, forall(v2, forall(v3, body)));
    }

    /**
     * Universal quantification with multiple variables.
     * Example: foralls(Arrays.asList("a", "b"), function(var("a"), var("b")))
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
     */
    static TypeScheme mono(Type body) {
        return new TypeScheme(Collections.emptyList(), body);
    }

    /**
     * Create a polymorphic type scheme with explicit type variables.
     * Example: poly(Arrays.asList("a", "b"), function(var("a"), var("b")))
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
     */
    static TypeScheme scheme(String var, Type body) {
        return new TypeScheme(Collections.singletonList(name(var)), body);
    }

    /**
     * Create a type scheme with two type variables.
     */
    static TypeScheme scheme(String var1, String var2, Type body) {
        return new TypeScheme(Arrays.asList(name(var1), name(var2)), body);
    }

    /**
     * Create a type scheme with three type variables.
     */
    static TypeScheme scheme(String var1, String var2, String var3, Type body) {
        return new TypeScheme(Arrays.asList(name(var1), name(var2), name(var3)), body);
    }

    /**
     * Create a type scheme with no type variables.
     */
    static TypeScheme scheme(Type body) {
        return mono(body);
    }

    // ===== Collection types =====

    /**
     * List type.
     * Example: list(string())
     */
    static Type list(Type elements) {
        return new Type.List(elements);
    }

    /**
     * List type (with string variable name).
     */
    static Type list(String elements) {
        return list(var(elements));
    }

    /**
     * Map/dictionary type with key and value types.
     * Example: map(string(), int32())
     */
    static Type map(Type keys, Type values) {
        return new Type.Map(new MapType(keys, values));
    }

    /**
     * Map type (with string variable names).
     */
    static Type map(String keys, String values) {
        return map(var(keys), var(values));
    }

    /**
     * Optional (nullable) type.
     * Example: optional(string())
     */
    static Type optional(Type elements) {
        return new Type.Maybe(elements);
    }

    /**
     * Optional type (with string variable name).
     */
    static Type optional(String elements) {
        return optional(var(elements));
    }

    /**
     * Set type.
     * Example: set(string())
     */
    static Type set(Type elements) {
        return new Type.Set(elements);
    }

    /**
     * Set type (with string variable name).
     */
    static Type set(String elements) {
        return set(var(elements));
    }

    /**
     * Flow type (monadic computation with state, trace, and error handling).
     * Example: {@literal flow(var("s"), var("x"))  // Flow<S, X>}
     */
    static Type flow(Type state, Type value) {
        return apply(apply(new Type.Variable(new Name("hydra.compute.Flow")), state), value);
    }

    /**
     * Flow type (with string variable names).
     * Example: {@literal flow("s", "x")  // Flow<S, X>}
     */
    static Type flow(String state, String value) {
        return flow(var(state), var(value));
    }

    /**
     * Flow type with Type state and String value variable.
     */
    static Type flow(String state, Type value) {
        return flow(var(state), value);
    }

    // ===== Product types =====

    /**
     * Create a pair type.
     * Example: pair(string(), int32())
     */
    static Type pair(Type first, Type second) {
        return new Type.Product(Arrays.asList(first, second));
    }

    /**
     * Create a 2-tuple type (alias for pair).
     */
    static Type tuple2(Type a, Type b) {
        return new Type.Product(Arrays.asList(a, b));
    }

    /**
     * Create a product type (tuple) with multiple components.
     * Example: product(Arrays.asList(string(), int32(), boolean()))
     */
    static Type product(List<Type> types) {
        return new Type.Product(types);
    }

    /**
     * Create a product type from varargs.
     */
    static Type product(Type... types) {
        return new Type.Product(Arrays.asList(types));
    }

    // ===== Sum types =====

    /**
     * Create an either type (a choice between two types).
     * Example: either(string(), int32())
     */
    static Type either(Type left, Type right) {
        return new Type.Sum(Arrays.asList(left, right));
    }

    /**
     * Create a sum type (disjoint union) with multiple variants.
     * Example: sum(Arrays.asList(string(), int32(), boolean()))
     */
    static Type sum(List<Type> types) {
        return new Type.Sum(types);
    }

    /**
     * Create a sum type from varargs.
     */
    static Type sum(Type... types) {
        return new Type.Sum(Arrays.asList(types));
    }

    // ===== Record and union types =====

    /**
     * Create a field with the given name and type.
     * Example: field("age", int32())
     */
    static FieldType field(String name, Type t) {
        return new FieldType(name(name), t);
    }

    /**
     * Create a row type.
     */
    static RowType row(Name name, List<FieldType> fields) {
        return new RowType(name, fields);
    }

    /**
     * Create a row type from varargs.
     */
    static RowType row(Name name, FieldType... fields) {
        return new RowType(name, Arrays.asList(fields));
    }

    /**
     * Create a record type with the given fields and the default type name.
     * Example: record(field("name", string()), field("age", int32()))
     */
    static Type record(FieldType... fields) {
        return recordWithName(PLACEHOLDER_NAME, fields);
    }

    /**
     * Create a record type with the given fields.
     */
    static Type record(List<FieldType> fields) {
        return recordWithName(PLACEHOLDER_NAME, fields);
    }

    /**
     * Create a record type with the given fields and a provided type name.
     * Example: recordWithName(name("Person"), Arrays.asList(field("name", string()), field("age", int32())))
     */
    static Type recordWithName(Name tname, List<FieldType> fields) {
        return new Type.Record(row(tname, fields));
    }

    /**
     * Create a record type with the given fields and a provided type name.
     */
    static Type recordWithName(Name tname, FieldType... fields) {
        return new Type.Record(row(tname, fields));
    }

    /**
     * Create a record type with the given name and fields.
     */
    static Type recordWithName(String tname, FieldType... fields) {
        return recordWithName(name(tname), fields);
    }

    /**
     * Unit type (empty record type).
     */
    static Type unit() {
        return new Type.Record(row(name("_Unit"), Collections.emptyList()));
    }

    /**
     * Create an enumerated type with the given variant names.
     * Example: enum_("red", "green", "blue")
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
     */
    static Type union(FieldType... fields) {
        return new Type.Union(row(PLACEHOLDER_NAME, fields));
    }

    /**
     * Create a union type with the given variants.
     */
    static Type union(List<FieldType> fields) {
        return new Type.Union(row(PLACEHOLDER_NAME, fields));
    }

    /**
     * Create a union type with the given variants and a provided type name.
     */
    static Type union(Name tname, FieldType... fields) {
        return new Type.Union(row(tname, fields));
    }

    /**
     * Create a union type with the given name and variants.
     */
    static Type union(String tname, FieldType... fields) {
        return union(name(tname), fields);
    }

    // ===== Wrapped types =====

    /**
     * Create a wrapped type (newtype) with a provided base type and the default type name.
     * Example: wrap(string())
     */
    static Type wrap(Type t) {
        return wrapWithName(PLACEHOLDER_NAME, t);
    }

    /**
     * Create a wrapped type (newtype) with a provided base type and type name.
     * Example: wrapWithName(name("Email"), string())
     */
    static Type wrapWithName(Name tname, Type t) {
        return new Type.Wrap(new WrappedType(tname, t));
    }

    /**
     * Create a wrapped type with the given name.
     */
    static Type wrapWithName(String tname, Type t) {
        return wrapWithName(name(tname), t);
    }

    // ===== Type variables =====

    /**
     * Create a type variable with the given name.
     * Example: var("a")
     */
    static Type var(String name) {
        return new Type.Variable(name(name));
    }

    /**
     * Create a type variable.
     */
    static Type variable(Name name) {
        return new Type.Variable(name);
    }

    /**
     * Create a type variable.
     */
    static Type variable(String name) {
        return var(name);
    }
}
