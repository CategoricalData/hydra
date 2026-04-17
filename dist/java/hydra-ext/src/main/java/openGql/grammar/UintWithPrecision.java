// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class UintWithPrecision implements Serializable, Comparable<UintWithPrecision> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.UintWithPrecision");

  public static final hydra.core.Name PRECISION = new hydra.core.Name("precision");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<String> precision;

  public final Boolean notNull;

  public UintWithPrecision (hydra.util.Maybe<String> precision, Boolean notNull) {
    this.precision = precision;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UintWithPrecision)) {
      return false;
    }
    UintWithPrecision o = (UintWithPrecision) other;
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
  public int compareTo(UintWithPrecision other) {
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

  public UintWithPrecision withPrecision(hydra.util.Maybe<String> precision) {
    return new UintWithPrecision(precision, notNull);
  }

  public UintWithPrecision withNotNull(Boolean notNull) {
    return new UintWithPrecision(precision, notNull);
  }
}
