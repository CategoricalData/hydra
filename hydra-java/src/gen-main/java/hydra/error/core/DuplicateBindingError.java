// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A duplicate binding name in a let expression
 */
public class DuplicateBindingError implements Serializable, Comparable<DuplicateBindingError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.DuplicateBindingError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the duplicate binding within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The duplicated binding name
   */
  public final hydra.core.Name name;

  public DuplicateBindingError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DuplicateBindingError)) {
      return false;
    }
    DuplicateBindingError o = (DuplicateBindingError) other;
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
  public int compareTo(DuplicateBindingError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      location,
      other.location);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }

  public DuplicateBindingError withLocation(hydra.paths.SubtermPath location) {
    return new DuplicateBindingError(location, name);
  }

  public DuplicateBindingError withName(hydra.core.Name name) {
    return new DuplicateBindingError(location, name);
  }
}
