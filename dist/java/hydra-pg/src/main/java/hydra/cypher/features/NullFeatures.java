// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.features;

import java.io.Serializable;

/**
 * IS NULL / IS NOT NULL checks
 */
public class NullFeatures implements Serializable, Comparable<NullFeatures> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.features.NullFeatures");

  public static final hydra.core.Name IS_NULL = new hydra.core.Name("isNull");

  public static final hydra.core.Name IS_NOT_NULL = new hydra.core.Name("isNotNull");

  /**
   * The IS NULL operator
   */
  public final Boolean isNull;

  /**
   * The IS NOT NULL operator
   */
  public final Boolean isNotNull;

  public NullFeatures (Boolean isNull, Boolean isNotNull) {
    this.isNull = isNull;
    this.isNotNull = isNotNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullFeatures)) {
      return false;
    }
    NullFeatures o = (NullFeatures) other;
    return java.util.Objects.equals(
      this.isNull,
      o.isNull) && java.util.Objects.equals(
      this.isNotNull,
      o.isNotNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(isNull) + 3 * java.util.Objects.hashCode(isNotNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NullFeatures other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      isNull,
      other.isNull);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      isNotNull,
      other.isNotNull);
  }

  public NullFeatures withIsNull(Boolean isNull) {
    return new NullFeatures(isNull, isNotNull);
  }

  public NullFeatures withIsNotNull(Boolean isNotNull) {
    return new NullFeatures(isNull, isNotNull);
  }
}
