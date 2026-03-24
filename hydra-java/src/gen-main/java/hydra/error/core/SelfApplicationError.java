// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A variable applied to itself, which is almost always a mistake in Hydra's type system (optional)
 */
public class SelfApplicationError implements Serializable, Comparable<SelfApplicationError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.SelfApplicationError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the self-application within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The name of the variable applied to itself
   */
  public final hydra.core.Name name;

  public SelfApplicationError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SelfApplicationError)) {
      return false;
    }
    SelfApplicationError o = (SelfApplicationError) other;
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
  public int compareTo(SelfApplicationError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public SelfApplicationError withLocation(hydra.paths.SubtermPath location) {
    return new SelfApplicationError(location, name);
  }

  public SelfApplicationError withName(hydra.core.Name name) {
    return new SelfApplicationError(location, name);
  }
}
