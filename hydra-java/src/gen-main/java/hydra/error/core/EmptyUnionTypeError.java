// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A union type with no alternatives; TypeVoid is preferred (optional)
 */
public class EmptyUnionTypeError implements Serializable, Comparable<EmptyUnionTypeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.EmptyUnionTypeError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the empty union type
   */
  public final hydra.accessors.AccessorPath location;

  public EmptyUnionTypeError (hydra.accessors.AccessorPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EmptyUnionTypeError)) {
      return false;
    }
    EmptyUnionTypeError o = (EmptyUnionTypeError) other;
    return java.util.Objects.equals(
      this.location,
      o.location);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EmptyUnionTypeError other) {
    return ((Comparable) location).compareTo(other.location);
  }
}
