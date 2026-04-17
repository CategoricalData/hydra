// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class IntegerWithPrecision implements Serializable, Comparable<IntegerWithPrecision> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.IntegerWithPrecision");

  public static final hydra.core.Name PRECISION = new hydra.core.Name("precision");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<String> precision;

  public final Boolean notNull;

  public IntegerWithPrecision (hydra.util.Maybe<String> precision, Boolean notNull) {
    this.precision = precision;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntegerWithPrecision)) {
      return false;
    }
    IntegerWithPrecision o = (IntegerWithPrecision) other;
    return java.util.Objects.equals(
      this.precision,
      o.precision) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(precision) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IntegerWithPrecision other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      precision,
      other.precision);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public IntegerWithPrecision withPrecision(hydra.util.Maybe<String> precision) {
    return new IntegerWithPrecision(precision, notNull);
  }

  public IntegerWithPrecision withNotNull(Boolean notNull) {
    return new IntegerWithPrecision(precision, notNull);
  }
}
