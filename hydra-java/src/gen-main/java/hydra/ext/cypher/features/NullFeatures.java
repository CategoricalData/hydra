// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.features;

import java.io.Serializable;

/**
 * IS NULL / IS NOT NULL checks
 */
public class NullFeatures implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/features.NullFeatures");
  
  public static final hydra.core.Name FIELD_NAME_IS_NULL = new hydra.core.Name("isNull");
  
  public static final hydra.core.Name FIELD_NAME_IS_NOT_NULL = new hydra.core.Name("isNotNull");
  
  /**
   * The IS NULL operator
   */
  public final Boolean isNull;
  
  /**
   * The IS NOT NULL operator
   */
  public final Boolean isNotNull;
  
  public NullFeatures (Boolean isNull, Boolean isNotNull) {
    java.util.Objects.requireNonNull((isNull));
    java.util.Objects.requireNonNull((isNotNull));
    this.isNull = isNull;
    this.isNotNull = isNotNull;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NullFeatures)) {
      return false;
    }
    NullFeatures o = (NullFeatures) (other);
    return isNull.equals(o.isNull) && isNotNull.equals(o.isNotNull);
  }
  
  @Override
  public int hashCode() {
    return 2 * isNull.hashCode() + 3 * isNotNull.hashCode();
  }
  
  public NullFeatures withIsNull(Boolean isNull) {
    java.util.Objects.requireNonNull((isNull));
    return new NullFeatures(isNull, isNotNull);
  }
  
  public NullFeatures withIsNotNull(Boolean isNotNull) {
    java.util.Objects.requireNonNull((isNotNull));
    return new NullFeatures(isNull, isNotNull);
  }
}
