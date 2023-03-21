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
import hydra.core.RowType;
import hydra.core.Type;
import hydra.core.UnitType;

import java.util.Arrays;
import java.util.Optional;

import static hydra.dsl.Core.*;


public interface Types {
    Name PLACEHOLDER_NAME = name("Placeholder");

    static <A> Type<A> annot(final A ann, final Type<A> base) {
        return new Type.Annotated<>(new Annotated<>(base, ann));
    }

    static <A> Type<A> apply(final Type<A> lhs, final Type<A> rhs) {
        return new Type.Application<>(new ApplicationType<>(lhs, rhs));
    }

    static <A> Type<A> bigfloat() {
        return literal(LiteralTypes.bigfloat());
    }

    static <A> Type<A> bigint() {
        return literal(LiteralTypes.bigint());
    }

    static <A> Type<A> binary() {
        return literal(LiteralTypes.binary());
    }

    static <A> Type<A> boolean_() {
        return literal(LiteralTypes.boolean_());
    }

    static <A> Type<A> enum_(final String... names) {
        FieldType<A>[] fields = new FieldType[names.length];
        for (int i = 0; i < names.length; i++) {
            fields[i] = field(names[i], unit());
        }

        return union(fields);
    }

    static <A> FieldType<A> field(final String name, final Type<A> t) {
        return new FieldType<>(new FieldName(name), t);
    }

    static <A> Type<A> float32() {
        return literal(LiteralTypes.float32());
    }

    static <A> Type<A> float64() {
        return literal(LiteralTypes.float64());
    }

    static <A> Type<A> float_(final FloatType ftype) {
        return literal(LiteralTypes.float_(ftype));
    }

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

    static <A> Type<A> function(final String dom, final Type<A> cod, final Type<A>... more) {
        return function(variable(dom), cod, more);
    }

    static <A> Type<A> function(final Type<A> dom, final String cod, final Type<A>... more) {
        return function(dom, variable(cod), more);
    }

    static <A> Type<A> function(final String dom, final String cod, final Type<A>... more) {
        return function(variable(dom), variable(cod), more);
    }

    static <A> Type<A> int16() {
        return literal(LiteralTypes.int16());
    }

    static <A> Type<A> int32() {
        return literal(LiteralTypes.int32());
    }

    static <A> Type<A> int64() {
        return literal(LiteralTypes.int64());
    }

    static <A> Type<A> int8() {
        return literal(LiteralTypes.int8());
    }

    static <A> Type<A> integer(final IntegerType itype) {
        return literal(LiteralTypes.integer(itype));
    }

    static <A> Type<A> lambda(final String var, final Type<A> body) {
        return new Type.Lambda<>(new LambdaType<>(new Name(var), body));
    }

    static <A> Type<A> lambda(final String var1, final String var2, final Type<A> body) {
        return lambda(var1, lambda(var2, body));
    }

    static <A> Type<A> list(final Type<A> elements) {
        return new Type.List<>(elements);
    }

    static <A> Type<A> list(final String elements) {
        return new Type.List<>(variable(elements));
    }

    static <A> Type<A> literal(final LiteralType ltype) {
        return new Type.Literal<>(ltype);
    }

    static <A> Type<A> map(final Type<A> keys, final Type<A> values) {
        return new Type.Map<>(new MapType<>(keys, values));
    }

    static <A> Type<A> map(final String keys, final String values) {
        return map(variable(keys), variable(values));
    }

    static <A> Type<A> optional(final Type<A> elements) {
        return new Type.Optional<>(elements);
    }

    static <A> Type<A> optional(final String elements) {
        return optional(variable(elements));
    }

    static <A> Type<A> record(final FieldType<A>... fields) {
        return record(PLACEHOLDER_NAME, fields);
    }

    static <A> Type<A> record(final Name name, final FieldType<A>... fields) {
        return new Type.Record<>(row(name, fields));
    }

    static <A> Type<A> record(final String name, final FieldType<A>... fields) {
        return record(name(name), fields);
    }

    static <A> RowType<A> row(final Name name, final FieldType<A>... fields) {
        return new RowType<>(name, Optional.empty(), Arrays.asList(fields));
    }

    static <A> Type<A> set(final Type<A> elements) {
        return new Type.Set<>(elements);
    }

    static <A> Type<A> set(final String elements) {
        return set(variable(elements));
    }

    static <A> Type<A> string() {
        return literal(LiteralTypes.string());
    }

    static <A> Type<A> union(final FieldType<A>... fields) {
        return union(PLACEHOLDER_NAME, fields);
    }

    static <A> Type<A> union(final Name name, final FieldType<A>... fields) {
        return new Type.Union<>(row(name, fields));
    }

    static <A> Type<A> union(final String name, final FieldType<A>... fields) {
        return union(name(name), fields);
    }

    static <A> Type<A> uint16() {
        return literal(LiteralTypes.uint16());
    }

    static <A> Type<A> uint32() {
        return literal(LiteralTypes.uint32());
    }

    static <A> Type<A> uint64() {
        return literal(LiteralTypes.uint64());
    }

    static <A> Type<A> uint8() {
        return literal(LiteralTypes.uint8());
    }

    static <A> Type<A> unit() {
        return record(UnitType.NAME);
    }

    static <A> Type<A> variable(final String name) {
        return new Type.Variable<>(new Name(name));
    }

    static <A> Type<A> wrap(final Name name) {
        return new Type.Wrap<>(name);
    }

    static <A> Type<A> wrap(final String name) {
        return wrap(name(name));
    }
}
