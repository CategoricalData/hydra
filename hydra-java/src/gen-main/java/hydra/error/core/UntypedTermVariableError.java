// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A term variable whose type is not known in the current scope
 */
public class UntypedTermVariableError implements Serializable, Comparable<UntypedTermVariableError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.UntypedTermVariableError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the untyped variable within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The name of the untyped variable
   */
  public final hydra.core.Name name;

  public UntypedTermVariableError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UntypedTermVariableError)) {
      return false;
    }
    UntypedTermVariableError o = (UntypedTermVariableError) other;
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
  public int compareTo(UntypedTermVariableError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public UntypedTermVariableError withLocation(hydra.paths.SubtermPath location) {
    return new UntypedTermVariableError(location, name);
  }

  public UntypedTermVariableError withName(hydra.core.Name name) {
    return new UntypedTermVariableError(location, name);
  }
}
