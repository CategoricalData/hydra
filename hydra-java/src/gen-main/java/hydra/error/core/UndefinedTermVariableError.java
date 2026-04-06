// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A variable reference to a term name that is not bound in scope
 */
public class UndefinedTermVariableError implements Serializable, Comparable<UndefinedTermVariableError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.UndefinedTermVariableError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the undefined variable within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The name of the undefined variable
   */
  public final hydra.core.Name name;

  public UndefinedTermVariableError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UndefinedTermVariableError)) {
      return false;
    }
    UndefinedTermVariableError o = (UndefinedTermVariableError) other;
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
  public int compareTo(UndefinedTermVariableError other) {
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

  public UndefinedTermVariableError withLocation(hydra.paths.SubtermPath location) {
    return new UndefinedTermVariableError(location, name);
  }

  public UndefinedTermVariableError withName(hydra.core.Name name) {
    return new UndefinedTermVariableError(location, name);
  }
}
