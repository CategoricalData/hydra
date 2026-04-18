// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class IntWithPrecision implements Serializable, Comparable<IntWithPrecision> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.IntWithPrecision");

  public static final hydra.core.Name PRECISION = new hydra.core.Name("precision");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<String> precision;

  public final Boolean notNull;

  public IntWithPrecision (hydra.util.Maybe<String> precision, Boolean notNull) {
    this.precision = precision;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntWithPrecision)) {
      return false;
    }
    IntWithPrecision o = (IntWithPrecision) other;
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
  public int compareTo(IntWithPrecision other) {
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

  public IntWithPrecision withPrecision(hydra.util.Maybe<String> precision) {
    return new IntWithPrecision(precision, notNull);
  }

  public IntWithPrecision withNotNull(Boolean notNull) {
    return new IntWithPrecision(precision, notNull);
  }
}
