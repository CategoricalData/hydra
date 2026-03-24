// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A duplicate field name in a record or union type
 */
public class DuplicateFieldError implements Serializable, Comparable<DuplicateFieldError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.DuplicateFieldError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the duplicate field within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The duplicated field name
   */
  public final hydra.core.Name name;

  public DuplicateFieldError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DuplicateFieldError)) {
      return false;
    }
    DuplicateFieldError o = (DuplicateFieldError) other;
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
  public int compareTo(DuplicateFieldError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public DuplicateFieldError withLocation(hydra.paths.SubtermPath location) {
    return new DuplicateFieldError(location, name);
  }

  public DuplicateFieldError withName(hydra.core.Name name) {
    return new DuplicateFieldError(location, name);
  }
}
