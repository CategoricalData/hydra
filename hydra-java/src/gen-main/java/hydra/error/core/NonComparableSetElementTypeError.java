// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A set type whose element type is or directly contains a function type, which cannot be compared for equality
 */
public class NonComparableSetElementTypeError implements Serializable, Comparable<NonComparableSetElementTypeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name ELEMENT_TYPE = new hydra.core.Name("elementType");

  /**
   * The path to the set type
   */
  public final hydra.accessors.AccessorPath location;

  /**
   * The non-comparable element type
   */
  public final hydra.core.Type elementType;

  public NonComparableSetElementTypeError (hydra.accessors.AccessorPath location, hydra.core.Type elementType) {
    this.location = location;
    this.elementType = elementType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonComparableSetElementTypeError)) {
      return false;
    }
    NonComparableSetElementTypeError o = (NonComparableSetElementTypeError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.elementType,
      o.elementType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(elementType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NonComparableSetElementTypeError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) elementType).compareTo(other.elementType);
  }

  public NonComparableSetElementTypeError withLocation(hydra.accessors.AccessorPath location) {
    return new NonComparableSetElementTypeError(location, elementType);
  }

  public NonComparableSetElementTypeError withElementType(hydra.core.Type elementType) {
    return new NonComparableSetElementTypeError(location, elementType);
  }
}
