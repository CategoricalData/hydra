package hydra.dsl;

import hydra.core.Annotated;
import hydra.core.ApplicationType;
import hydra.core.FieldName;
import hydra.core.FieldType;
import hydra.core.FloatType;
import hydra.core.FunctionType;
import hydra.core.IntegerType;
import hydra.core.LambdaType;
import hydra.core.LiteralType;
import hydra.core.MapType;
import hydra.core.Name;
import hydra.core.Nominal;
import hydra.core.RowType;
import hydra.core.Type;
import hydra.core.Unit;
import hydra.compute.Flow;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static hydra.dsl.Core.name;


/**
 * DSL utilities for constructing Hydra types.
 */
public interface Types {
    Name PLACEHOLDER_NAME = name("Placeholder");

    /**
     * Construct an annotated type.
     */
    static <A> Type<A> annot(final A ann, final Type<A> base) {
        return new Type.Annotated<>(new Annotated<>(base, ann));
    }

    /**
     * Construct an application type.
     */
    static <A> Type<A> apply(final Type<A> lhs, final Type<A>... rhs) {
        Type<A> cur = lhs;
        for (Type<A> r : rhs) {
            cur = new Type.Application<>(new ApplicationType<>(cur, r));
        }
        return cur;
    }

    /**
     * Construct the bigfloat literal type.
     */
    static <A> Type<A> bigfloat() {
        return literal(LiteralTypes.bigfloat());
    }

    /**
     * Construct the bigint literal type.
     */
    static <A> Type<A> bigint() {
        return literal(LiteralTypes.bigint());
    }

    /**
     * Construct the binary literal type.
     */
    static <A> Type<A> binary() {
        return literal(LiteralTypes.binary());
    }

    /**
     * Construct the boolean literal type.
     */
    static <A> Type<A> boolean_() {
        return literal(LiteralTypes.boolean_());
    }

    /**
     * Construct an enumerated type.
     */
    static <A> Type<A> enum_(final String... names) {
        FieldType<A>[] fields = new FieldType[names.length];
        for (int i = 0; i < names.length; i++) {
            fields[i] = field(names[i], unit());
        }

        return union(fields);
    }

    /**
     * Construct a field type.
     */
    static <A> FieldType<A> field(final String name, final Type<A> t) {
        return new FieldType<>(new FieldName(name), t);
    }

    /**
     * Construct the float32 literal type.
     */
    static <A> Type<A> float32() {
        return literal(LiteralTypes.float32());
    }

    /**
     * Construct the float64 literal type.
     */
    static <A> Type<A> float64() {
        return literal(LiteralTypes.float64());
    }

    /**
     * Construct a float literal type.
     */
    static <A> Type<A> float_(final FloatType ftype) {
        return literal(LiteralTypes.float_(ftype));
    }

    /**
     * Construct a flow type.
     */
    static <A> Type<A> flow(final Type<A> states, final Type<A> elements) {
        return apply(variable(Flow.NAME), states, elements);
    }

    /**
     * Construct a flow type.
     */
    static <A> Type<A> flow(final Type<A> states, final String elements) {
        return apply(variable(Flow.NAME), states, variable(elements));
    }

    /**
     * Construct a flow type.
     */
    static <A> Type<A> flow(final String states, final Type<A> elements) {
        return apply(variable(Flow.NAME), variable(states), elements);
    }

    /**
     * Construct a flow type.
     */
    static <A> Type<A> flow(final String states, final String elements) {
        return flow(variable(states), variable(elements));
    }

    /**
     * Construct a function type.
     */
    static <A> Type<A> function(final Type<A> dom, final Type<A> cod, final Type<A>... more) {
        Type<A> c;

        if (more.length == 0) {
            c = cod;
        } else {
            c = more[more.length - 1];
            for (int i = more.length - 2; i >= 0; i--) {
                c = new Type.Function<>(new FunctionType<>(more[i], c));
            }
            c = new Type.Function<>(new FunctionType<>(cod, c));
        }

        return new Type.Function<>(new FunctionType<>(dom, c));
    }

    /**
     * Construct a function type.
     */
    static <A> Type<A> function(final String dom, final Type<A> cod, final Type<A>... more) {
        return function(variable(dom), cod, more);
    }

    /**
     * Construct a function type.
     */
    static <A> Type<A> function(final Type<A> dom, final String cod, final Type<A>... more) {
        return function(dom, variable(cod), more);
    }

    /**
     * Construct a function type.
     */
    static <A> Type<A> function(final String dom, final String cod, final Type<A>... more) {
        return function(variable(dom), variable(cod), more);
    }

    /**
     * Construct a function type.
     */
    static <A> Type<A> function(final String dom1, final String dom2, final String dom3, final Type<A>... cod) {
        return function(variable(dom1), function(variable(dom2), variable(dom3), cod));
    }

    /**
     * Construct the int16 literal type.
     */
    static <A> Type<A> int16() {
        return literal(LiteralTypes.int16());
    }

    /**
     * Construct the int32 literal type.
     */
    static <A> Type<A> int32() {
        return literal(LiteralTypes.int32());
    }

    /**
     * Construct the int64 literal type.
     */
    static <A> Type<A> int64() {
        return literal(LiteralTypes.int64());
    }

    /**
     * Construct the int8 literal type.
     */
    static <A> Type<A> int8() {
        return literal(LiteralTypes.int8());
    }

    /**
     * Construct an integer type.
     */
    static <A> Type<A> integer(final IntegerType itype) {
        return literal(LiteralTypes.integer(itype));
    }

    /**
     * Construct a lambda (universal) type.
     */
    static <A> Type<A> lambda(final String var, final Type<A> body) {
        return new Type.Lambda<>(new LambdaType<>(new Name(var), body));
    }

    /**
     * Construct a lambda (universal) type.
     */
    static <A> Type<A> lambda(final String var1, final String var2, final Type<A> body) {
        return lambda(var1, lambda(var2, body));
    }

    /**
     * Construct a lambda (universal) type.
     */
    static <A> Type<A> lambda(final String var1, final String var2, final String var3, final Type<A> body) {
        return lambda(var1, lambda(var2, lambda(var3, body)));
    }

    /**
     * Construct a list type.
     */
    static <A> Type<A> list(final Type<A> elements) {
        return new Type.List<>(elements);
    }

    /**
     * Construct a list type.
     */
    static <A> Type<A> list(final String elements) {
        return new Type.List<>(variable(elements));
    }

    /**
     * Construct a literal type.
     */
    static <A> Type<A> literal(final LiteralType ltype) {
        return new Type.Literal<>(ltype);
    }

    /**
     * Construct a map type.
     */
    static <A> Type<A> map(final Type<A> keys, final Type<A> values) {
        return new Type.Map<>(new MapType<>(keys, values));
    }

    /**
     * Construct a map type.
     */
    static <A> Type<A> map(final String keys, final String values) {
        return map(variable(keys), variable(values));
    }

    /**
     * Construct an optional type.
     */
    static <A> Type<A> optional(final Type<A> elements) {
        return new Type.Optional<>(elements);
    }

    /**
     * Construct an optional type.
     */
    static <A> Type<A> optional(final String elements) {
        return optional(variable(elements));
    }

    /**
     * Construct a pair type.
     */
    static <A> Type<A> pair(final Type<A> fst, final Type<A> snd) {
        List<Type<A>> types = new ArrayList<>();
        types.add(fst);
        types.add(snd);
        return new Type.Product<>(types);
    }

    /**
     * Construct a record type.
     */
    static <A> Type<A> record(final FieldType<A>... fields) {
        return record(PLACEHOLDER_NAME, fields);
    }

    /**
     * Construct a record type.
     */
    static <A> Type<A> record(final Name name, final FieldType<A>... fields) {
        return new Type.Record<>(row(name, fields));
    }

    /**
     * Construct a record type.
     */
    static <A> Type<A> record(final String name, final FieldType<A>... fields) {
        return record(name(name), fields);
    }

    /**
     * Construct a row type.
     */
    static <A> RowType<A> row(final Name name, final FieldType<A>... fields) {
        return new RowType<>(name, Optional.empty(), Arrays.asList(fields));
    }

    /**
     * Construct a set type.
     */
    static <A> Type<A> set(final Type<A> elements) {
        return new Type.Set<>(elements);
    }

    /**
     * Construct a set type.
     */
    static <A> Type<A> set(final String elements) {
        return set(variable(elements));
    }

    /**
     * Construct the string literal type.
     */
    static <A> Type<A> string() {
        return literal(LiteralTypes.string());
    }

    /**
     * Construct a union type.
     */
    static <A> Type<A> union(final FieldType<A>... fields) {
        return union(PLACEHOLDER_NAME, fields);
    }

    /**
     * Construct a union type.
     */
    static <A> Type<A> union(final Name name, final FieldType<A>... fields) {
        return new Type.Union<>(row(name, fields));
    }

    /**
     * Construct a union type.
     */
    static <A> Type<A> union(final String name, final FieldType<A>... fields) {
        return union(name(name), fields);
    }

    /**
     * Construct the uint16 literal type.
     */
    static <A> Type<A> uint16() {
        return literal(LiteralTypes.uint16());
    }

    /**
     * Construct the uint32 literal type.
     */
    static <A> Type<A> uint32() {
        return literal(LiteralTypes.uint32());
    }

    /**
     * Construct the uint64 literal type.
     */
    static <A> Type<A> uint64() {
        return literal(LiteralTypes.uint64());
    }

    /**
     * Construct the uint8 literal type.
     */
    static <A> Type<A> uint8() {
        return literal(LiteralTypes.uint8());
    }

    /**
     * Construct the unit type.
     */
    static <A> Type<A> unit() {
        return record(Unit.NAME);
    }

    /**
     * Construct a type variable.
     */
    static <A> Type<A> variable(final Name name) {
        return new Type.Variable<>(name);
    }

    /**
     * Construct a type variable.
     */
    static <A> Type<A> variable(final String name) {
        return variable(new Name(name));
    }

    /**
     * Construct a wrapper type.
     */
    static <A> Type<A> wrap(final Name name, final Type<A> type) {
        return new Type.Wrap<>(new Nominal<>(name, type));
    }

    /**
     * Construct a wrapper type.
     */
    static <A> Type<A> wrap(final String name, final Type<A> type) {
        return wrap(name(name), type);
    }
}
