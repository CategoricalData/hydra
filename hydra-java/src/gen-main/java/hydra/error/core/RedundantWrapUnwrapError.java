// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * An unwrap elimination applied to a wrap term of the same type, forming a no-op round-trip (optional)
 */
public class RedundantWrapUnwrapError implements Serializable, Comparable<RedundantWrapUnwrapError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  /**
   * The path to the redundant wrap/unwrap within the term
   */
  public final hydra.accessors.AccessorPath location;

  /**
   * The type name of the wrapper
   */
  public final hydra.core.Name typeName;

  public RedundantWrapUnwrapError (hydra.accessors.AccessorPath location, hydra.core.Name typeName) {
    this.location = location;
    this.typeName = typeName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RedundantWrapUnwrapError)) {
      return false;
    }
    RedundantWrapUnwrapError o = (RedundantWrapUnwrapError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.typeName,
      o.typeName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(typeName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RedundantWrapUnwrapError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) typeName).compareTo(other.typeName);
  }

  public RedundantWrapUnwrapError withLocation(hydra.accessors.AccessorPath location) {
    return new RedundantWrapUnwrapError(location, typeName);
  }

  public RedundantWrapUnwrapError withTypeName(hydra.core.Name typeName) {
    return new RedundantWrapUnwrapError(location, typeName);
  }
}
