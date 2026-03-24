// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A lambda parameter or let binding name that shadows a variable already in scope (optional)
 */
public class TermVariableShadowingError implements Serializable, Comparable<TermVariableShadowingError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.TermVariableShadowingError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the shadowing binding within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The name of the shadowed variable
   */
  public final hydra.core.Name name;

  public TermVariableShadowingError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermVariableShadowingError)) {
      return false;
    }
    TermVariableShadowingError o = (TermVariableShadowingError) other;
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
  public int compareTo(TermVariableShadowingError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public TermVariableShadowingError withLocation(hydra.paths.SubtermPath location) {
    return new TermVariableShadowingError(location, name);
  }

  public TermVariableShadowingError withName(hydra.core.Name name) {
    return new TermVariableShadowingError(location, name);
  }
}
