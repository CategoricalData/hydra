// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A map type whose key type is or directly contains a function type, which cannot be compared for equality
 */
public class NonComparableMapKeyTypeError implements Serializable, Comparable<NonComparableMapKeyTypeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name KEY_TYPE = new hydra.core.Name("keyType");

  /**
   * The path to the map type
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The non-comparable key type
   */
  public final hydra.core.Type keyType;

  public NonComparableMapKeyTypeError (hydra.paths.SubtermPath location, hydra.core.Type keyType) {
    this.location = location;
    this.keyType = keyType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonComparableMapKeyTypeError)) {
      return false;
    }
    NonComparableMapKeyTypeError o = (NonComparableMapKeyTypeError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.keyType,
      o.keyType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(keyType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NonComparableMapKeyTypeError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) keyType).compareTo(other.keyType);
  }

  public NonComparableMapKeyTypeError withLocation(hydra.paths.SubtermPath location) {
    return new NonComparableMapKeyTypeError(location, keyType);
  }

  public NonComparableMapKeyTypeError withKeyType(hydra.core.Type keyType) {
    return new NonComparableMapKeyTypeError(location, keyType);
  }
}
