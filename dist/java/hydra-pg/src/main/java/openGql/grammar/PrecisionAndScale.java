// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class PrecisionAndScale implements Serializable, Comparable<PrecisionAndScale> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PrecisionAndScale");

  public static final hydra.core.Name PRECISION = new hydra.core.Name("precision");

  public static final hydra.core.Name SCALE = new hydra.core.Name("scale");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final String precision;

  public final hydra.util.Maybe<String> scale;

  public final Boolean notNull;

  public PrecisionAndScale (String precision, hydra.util.Maybe<String> scale, Boolean notNull) {
    this.precision = precision;
    this.scale = scale;
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PrecisionAndScale)) {
      return false;
    }
    PrecisionAndScale o = (PrecisionAndScale) other;
    return java.util.Objects.equals(
      this.precision,
      o.precision) && java.util.Objects.equals(
      this.scale,
      o.scale) && java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(precision) + 3 * java.util.Objects.hashCode(scale) + 5 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PrecisionAndScale other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      precision,
      other.precision);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      scale,
      other.scale);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }

  public PrecisionAndScale withPrecision(String precision) {
    return new PrecisionAndScale(precision, scale, notNull);
  }

  public PrecisionAndScale withScale(hydra.util.Maybe<String> scale) {
    return new PrecisionAndScale(precision, scale, notNull);
  }

  public PrecisionAndScale withNotNull(Boolean notNull) {
    return new PrecisionAndScale(precision, scale, notNull);
  }
}
