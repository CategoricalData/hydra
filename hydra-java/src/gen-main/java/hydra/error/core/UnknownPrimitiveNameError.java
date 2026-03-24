// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A primitive function reference to a name not in the known primitive registry
 */
public class UnknownPrimitiveNameError implements Serializable, Comparable<UnknownPrimitiveNameError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the primitive reference within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The unknown primitive name
   */
  public final hydra.core.Name name;

  public UnknownPrimitiveNameError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnknownPrimitiveNameError)) {
      return false;
    }
    UnknownPrimitiveNameError o = (UnknownPrimitiveNameError) other;
    return java.util.Objects.equals(
      this.location,
      o.location) && java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location) + 3 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnknownPrimitiveNameError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public UnknownPrimitiveNameError withLocation(hydra.paths.SubtermPath location) {
    return new UnknownPrimitiveNameError(location, name);
  }

  public UnknownPrimitiveNameError withName(hydra.core.Name name) {
    return new UnknownPrimitiveNameError(location, name);
  }
}
