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

  static <M> Type<M> annot(final M ann, final Type<M> base) {
    return new Type.Annotated<>(new Annotated<>(base, ann));
  }

  static <M> Type<M> apply(final Type<M> lhs, final Type<M> rhs) {
    return new Type.Application<>(new ApplicationType<>(lhs, rhs));
  }

  static <M> Type<M> bigfloat() {
    return literal(LiteralTypes.bigfloat());
  }

  static <M> Type<M> bigint() {
    return literal(LiteralTypes.bigint());
  }

  static <M> Type<M> binary() {
    return literal(LiteralTypes.binary());
  }

  static <M> Type<M> boolean_() {
    return literal(LiteralTypes.boolean_());
  }

  static <M> Type<M> element(final Type<M> other) {
    return new Type.Element<>(other);
  }

  static <M> Type<M> enum_(final String... names) {
    FieldType<M>[] fields = new FieldType[names.length];
    for (int i = 0; i < names.length; i++) {
      fields[i] = field(names[i], unit());
    }

    return union(fields);
  }

  static <M> FieldType<M> field(final String name, final Type<M> t) {
    return new FieldType<>(new FieldName(name), t);
  }

  static <M> Type<M> float32() {
    return literal(LiteralTypes.float32());
  }

  static <M> Type<M> float64() {
    return literal(LiteralTypes.float64());
  }

  static <M> Type<M> float_(final FloatType ftype) {
    return literal(LiteralTypes.float_(ftype));
  }

  static <M> Type<M> function(final Type<M> dom, final Type<M> cod, final Type<M>... more) {
    Type<M> c;

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

  static <M> Type<M> int16() {
    return literal(LiteralTypes.int16());
  }

  static <M> Type<M> int32() {
    return literal(LiteralTypes.int32());
  }

  static <M> Type<M> int64() {
    return literal(LiteralTypes.int64());
  }

  static <M> Type<M> int8() {
    return literal(LiteralTypes.int8());
  }

  static <M> Type<M> integer(final IntegerType itype) {
    return literal(LiteralTypes.integer(itype));
  }

  static <M> Type<M> lambda(final String var, final Type<M> body) {
    return new Type.Lambda<>(new LambdaType<>(new Name(var), body));
  }

  static <M> Type<M> list(final Type<M> elements) {
    return new Type.List<>(elements);
  }

  static <M> Type<M> literal(final LiteralType ltype) {
    return new Type.Literal<>(ltype);
  }

  static <M> Type<M> map(final Type<M> keys, final Type<M> values) {
    return new Type.Map<>(new MapType<>(keys, values));
  }

  static <M> Type<M> optional(final Type<M> elements) {
    return new Type.Optional<>(elements);
  }

  static <M> Type<M> record(final FieldType<M>... fields) {
    return record(PLACEHOLDER_NAME, fields);
  }

  static <M> Type<M> record(final Name name, final FieldType<M>... fields) {
    return new Type.Record<>(row(name, fields));
  }

  static <M> Type<M> record(final String name, final FieldType<M>... fields) {
    return record(name(name), fields);
  }

  static <M> RowType<M> row(final Name name, final FieldType<M>... fields) {
    return new RowType<>(name, Optional.empty(), Arrays.asList(fields));
  }

  static <M> Type<M> set(final Type<M> elements) {
    return new Type.Set<>(elements);
  }

  static <M> Type<M> string() {
    return literal(LiteralTypes.string());
  }

  static <M> Type<M> union(final FieldType<M>... fields) {
    return union(PLACEHOLDER_NAME, fields);
  }

  static <M> Type<M> union(final Name name, final FieldType<M>... fields) {
    return new Type.Union<>(row(name, fields));
  }

  static <M> Type<M> union(final String name, final FieldType<M>... fields) {
    return union(name(name), fields);
  }

  static <M> Type<M> uint16() {
    return literal(LiteralTypes.uint16());
  }

  static <M> Type<M> uint32() {
    return literal(LiteralTypes.uint32());
  }

  static <M> Type<M> uint64() {
    return literal(LiteralTypes.uint64());
  }

  static <M> Type<M> uint8() {
    return literal(LiteralTypes.uint8());
  }

  static <M> Type<M> unit() {
    return record(UnitType.NAME);
  }

  static <M> Type<M> variable(final String name) {
    return new Type.Variable<>(new Name(name));
  }

  static <M> Type<M> wrap(final Name name) {
    return new Type.Wrap<>(name);
  }

  static <M> Type<M> wrap(final String name) {
    return wrap(name(name));
  }
}
