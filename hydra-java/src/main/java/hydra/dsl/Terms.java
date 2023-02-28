package hydra.dsl;

import hydra.core.Annotated;
import hydra.core.Application;
import hydra.core.CaseStatement;
import hydra.core.Elimination;
import hydra.core.Field;
import hydra.core.FieldName;
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
import hydra.core.UnitType;

import hydra.core.Nominal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static hydra.dsl.Core.*;


public interface Terms {
  static <M> Term<M> annot(final M ann, final Term<M> base) {
    return new Term.Annotated<>(new Annotated<>(base, ann));
  }

  static <M> Term<M> apply(final Term<M> lhs, final Term<M> rhs) {
    return new Term.Application<>(new Application<>(lhs, rhs));
  }

  static <M> Term<M> bigfloat(final double value) {
    return float_(new FloatValue.Bigfloat(value));
  }

  static <M> Term<M> bigint(final BigInteger value) {
    return integer(new IntegerValue.Bigint(value));
  }

  static <M> Term<M> binary(final String value) {
    return literal(new Literal.Binary(value));
  }

  static <M> Term<M> boolean_(final boolean value) {
    return literal(new Literal.Boolean_(value));
  }

  static <M> Term<M> cases(final Name name, final Field<M>... fields) {
    return elimination(new Elimination.Union<>(new CaseStatement<>(name, Arrays.asList(fields))));
  }

  static <M> Term<M> delta() {
    return elimination(new Elimination.Element<>());
  }

  static <M> Term<M> elimination(final Elimination<M> elim) {
    return function(new Function.Elimination<>(elim));
  }

  static <M> Field<M> field(final String fname, final Term<M> term) {
    return new Field<>(new FieldName(fname), term);
  }

  static <M> Term<M> float_(final FloatValue value) {
    return literal(new Literal.Float_(value));
  }

  static <M> Term<M> float32(final float value) {
    return float_(new FloatValue.Float32(value));
  }

  static <M> Term<M> float64(final double value) {
    return float_(new FloatValue.Float64(value));
  }

  static <M> Term<M> function(final Function<M> fun) {
    return new Term.Function<>(fun);
  }

  static <M> Term<M> int8(final short value) {
    return integer(new IntegerValue.Int8(value));
  }

  static <M> Term<M> int16(final short value) {
    return integer(new IntegerValue.Int16(value));
  }

  static <M> Term<M> int32(final int value) {
    return integer(new IntegerValue.Int32(value));
  }

  static <M> Term<M> int64(final long value) {
    return integer(new IntegerValue.Int64(value));
  }

  static <M> Term<M> integer(final IntegerValue value) {
    return literal(new Literal.Integer_(value));
  }

  static <M> Term<M> lambda(final String var, final Term<M> body) {
    return function(new Function.Lambda<>(new Lambda<>(new Name(var), body)));
  }

  static <M> Term<M> let(final String var, final Term<M> defined, final Term<M> definedIn) {
    Map<Name, Term<M>> bindings = new HashMap<>();
    bindings.put(new Name(var), defined);
    return new Term.Let<>(new Let<>(bindings, definedIn));
  }

  static <M> Term<M> list(final Term<M>... elements) {
    return new Term.List<>(Arrays.asList(elements));
  }

  static <M> Term<M> literal(final Literal value) {
    return new Term.Literal<>(value);
  }

  static <M> Term<M> map(final Map<Term<M>, Term<M>> value) {
    return new Term.Map<>(value);
  }

  static <M> Term<M> match(final Name name, final Map.Entry<String, Term<M>>... casePairs) {
    Field<M>[] fields = new Field[casePairs.length];
    for (int i = 0; i < casePairs.length; i++) {
      fields[i] = field(casePairs[i].getKey(), casePairs[i].getValue());
    }
    return cases(name, fields);
  }

  static <M> Term<M> optional(final Optional<Term<M>> maybeTerm) {
    return new Term.Optional<>(maybeTerm);
  }

  static <M> Term<M> primitive(final Name primName) {
    return function(new Function.Primitive<>(primName));
  }

  static <M> Term<M> primitive(final String primName) {
    return primitive(name(primName));
  }

  static <M> Term<M> projection(final Name recordName, final FieldName fname) {
    return elimination(new Elimination.Record<>(new Projection(recordName, fname)));
  }

  static <M> Term<M> projection(final String recordName, final String fname) {
    return projection(name(recordName), new FieldName(fname));
  }

  static <M> Term<M> record(final Name recordName, final Field<M>... fields) {
    return new Term.Record<>(new Record<>(recordName, Arrays.asList(fields)));
  }

  static <M> Term<M> record(final String recordName, final Field<M>... fields) {
    return record(name(recordName), fields);
  }

  static <M> Term<M> set(final Set<Term<M>> value) {
    return new Term.Set<>(value);
  }

  static <M> Term<M> string(final String value) {
    return literal(new Literal.String_(value));
  }

  static <M> Term<M> uint8(final byte value) {
    return integer(new IntegerValue.Uint8(value));
  }

  static <M> Term<M> uint16(final char value) {
    return integer(new IntegerValue.Uint16(value));
  }

  static <M> Term<M> uint32(final long value) {
    return integer(new IntegerValue.Uint32(value));
  }

  static <M> Term<M> uint64(final BigInteger value) {
    return integer(new IntegerValue.Uint64(value));
  }

  static <M> Term<M> inject(final Name unionName, final Field<M> field) {
    return new Term.Union<>(new Injection<>(unionName, field));
  }

  static <M> Term<M> inject(final String unionName, final Field<M> field) {
    return inject(name(unionName), field);
  }

  static <M> Term<M> unit() {
    return record(UnitType.NAME);
  }

  static <M> Term<M> variable(final String var) {
    return new Term.Variable<>(new Name(var));
  }

  static <M> Term<M> wrap(final Name name, final Term<M> term) {
    return new Term.Wrap<>(new Nominal<>(name, term));
  }
}
