// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * TypeVoid appearing in a position where no value can be constructed, such as a record field, list element, map key/value, set element, pair component, or function codomain (optional)
 */
public class VoidInNonBottomPositionError implements Serializable, Comparable<VoidInNonBottomPositionError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the void type in a non-bottom position
   */
  public final hydra.accessors.AccessorPath location;

  public VoidInNonBottomPositionError (hydra.accessors.AccessorPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VoidInNonBottomPositionError)) {
      return false;
    }
    VoidInNonBottomPositionError o = (VoidInNonBottomPositionError) other;
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
  public int compareTo(VoidInNonBottomPositionError other) {
    return ((Comparable) location).compareTo(other.location);
  }
}
