// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class FloatTypeWithPrecision implements Serializable, Comparable<FloatTypeWithPrecision> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FloatTypeWithPrecision");

  public static final hydra.core.Name PRECISION_AND_SCALE = new hydra.core.Name("precisionAndScale");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final hydra.util.Maybe<openGql.grammar.PrecisionAndScale> precisionAndScale;

  public final Boolean notNull;

  public FloatTypeWithPrecision (hydra.util.Maybe<openGql.grammar.PrecisionAndScale> precisionAndScale, Boolean notNull) {
    this.precisionAndScale = precisionAndScale;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FloatTypeWithPrecision)) {
      return false;
    }
    FloatTypeWithPrecision o = (FloatTypeWithPrecision) other;
    return java.util.Objects.equals(
      this.precisionAndScale,
      o.precisionAndScale) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(precisionAndScale) + 3 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FloatTypeWithPrecision other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      precisionAndScale,
      other.precisionAndScale);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public FloatTypeWithPrecision withPrecisionAndScale(hydra.util.Maybe<openGql.grammar.PrecisionAndScale> precisionAndScale) {
    return new FloatTypeWithPrecision(precisionAndScale, notNull);
  }

  public FloatTypeWithPrecision withNotNull(Boolean notNull) {
    return new FloatTypeWithPrecision(precisionAndScale, notNull);
  }
}
