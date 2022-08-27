package hydra.impl.java.dsl;

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
import hydra.core.VariableType;

import java.util.Arrays;


public class Types {
    private static final Name PLACEHOLDER_NAME = name("Placeholder");

    public static <M> Type<M> annot(final M ann, final Type<M> base) {
        return new Type.Annotated<>(new Annotated<>(base, ann));
    }

    public static <M> Type<M> apply(final Type<M> lhs, final Type<M> rhs) {
        return new Type.Application<>(new ApplicationType<>(lhs, rhs));
    }

    public static <M> Type<M> bigfloat() {
        return float_(new FloatType.Bigfloat());
    }

    public static <M> Type<M> bigint() {
        return integer(new IntegerType.Bigint());
    }

    public static <M> Type<M> binary() {
        return literal(new LiteralType.Binary());
    }

    public static <M> Type<M> boolean_() {
        return literal(new LiteralType.Boolean_());
    }

    public static <M> Type<M> element(final Type<M> other) {
        return new Type.Element<>(other);
    }

    public static <M> Type<M> enum_(final String... names) {
        FieldType<M>[] fields = new FieldType[names.length];
        for (int i = 0; i < names.length; i++) {
            fields[i] = field(names[i], unit());
        }

        return union(fields);
    }

    public static <M> FieldType<M> field(final String name, final Type<M> t) {
        return new FieldType<>(new FieldName(name), t);
    }

    public static Name name(final String name) {
        return new Name(name);
    }

    public static <M> Type<M> float32() {
        return float_(new FloatType.Float32());
    }

    public static <M> Type<M> float64() {
        return float_(new FloatType.Float64());
    }

    public static <M> Type<M> float_(final FloatType ftype) {
        return literal(new LiteralType.Float_(ftype));
    }

    public static <M> Type<M> function(final Type<M> dom, final Type<M> cod) {
        return new Type.Function<>(new FunctionType<>(dom, cod));
    }

    public static <M> Type<M> int16() {
        return integer(new IntegerType.Int16());
    }

    public static <M> Type<M> int32() {
        return integer(new IntegerType.Int32());
    }

    public static <M> Type<M> int64() {
        return integer(new IntegerType.Int64());
    }

    public static <M> Type<M> int8() {
        return integer(new IntegerType.Int8());
    }

    public static <M> Type<M> integer(final IntegerType itype) {
        return literal(new LiteralType.Integer_(itype));
    }

    public static <M> Type<M> lambda(final String var, final Type<M> body) {
        return new Type.Lambda<>(new LambdaType<>(new VariableType(var), body));
    }

    public static <M> Type<M> list(final Type<M> elements) {
        return new Type.List<>(elements);
    }

    public static <M> Type<M> literal(final LiteralType ltype) {
        return new Type.Literal<>(ltype);
    }

    public static <M> Type<M> map(final Type<M> keys, final Type<M> values) {
        return new Type.Map<>(new MapType<>(keys, values));
    }

    public static <M> Type<M> nominal(final Name name) {
        return new Type.Nominal<>(name);
    }

    public static <M> Type<M> optional(final Type<M> elements) {
        return new Type.Optional<>(elements);
    }

    public static <M> Type<M> record(final FieldType<M>... fields) {
        return record(PLACEHOLDER_NAME, fields);
    }

    public static <M> RowType<M> row(final Name name, final FieldType<M>... fields) {
        return new RowType<>(name, Arrays.asList(fields));
    }

    public static <M> Type<M> set(final Type<M> elements) {
        return new Type.Set<>(elements);
    }

    public static <M> Type<M> record(final Name name, final FieldType<M>... fields) {
        return new Type.Record<>(row(name, fields));
    }

    public static <M> Type<M> union(final FieldType<M>... fields) {
        return union(PLACEHOLDER_NAME, fields);
    }

    public static <M> Type<M> union(final Name name, final FieldType<M>... fields) {
        return new Type.Union<>(row(name, fields));
    }

    public static <M> Type<M> uint16() {
        return integer(new IntegerType.Uint16());
    }

    public static <M> Type<M> uint32() {
        return integer(new IntegerType.Uint32());
    }

    public static <M> Type<M> uint64() {
        return integer(new IntegerType.Uint64());
    }

    public static <M> Type<M> uint8() {
        return integer(new IntegerType.Uint8());
    }

    public static <M> Type<M> unit() {
        return record(UnitType.NAME);
    }

    public static <M> Type<M> variable(final String name) {
        return new Type.Variable<>(new VariableType(name));
    }
}
