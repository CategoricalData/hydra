// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A record type with no fields; TypeUnit is preferred for the unit-like case (optional)
 */
public class EmptyRecordTypeError implements Serializable, Comparable<EmptyRecordTypeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.EmptyRecordTypeError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the empty record type
   */
  public final hydra.accessors.AccessorPath location;

  public EmptyRecordTypeError (hydra.accessors.AccessorPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EmptyRecordTypeError)) {
      return false;
    }
    EmptyRecordTypeError o = (EmptyRecordTypeError) other;
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
  public int compareTo(EmptyRecordTypeError other) {
    return ((Comparable) location).compareTo(other.location);
  }
}
