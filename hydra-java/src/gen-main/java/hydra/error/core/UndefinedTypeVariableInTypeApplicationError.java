// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A type variable in a type application term that is not bound in scope
 */
public class UndefinedTypeVariableInTypeApplicationError implements Serializable, Comparable<UndefinedTypeVariableInTypeApplicationError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the type application within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The name of the undefined type variable
   */
  public final hydra.core.Name name;

  public UndefinedTypeVariableInTypeApplicationError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UndefinedTypeVariableInTypeApplicationError)) {
      return false;
    }
    UndefinedTypeVariableInTypeApplicationError o = (UndefinedTypeVariableInTypeApplicationError) other;
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
  public int compareTo(UndefinedTypeVariableInTypeApplicationError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public UndefinedTypeVariableInTypeApplicationError withLocation(hydra.paths.SubtermPath location) {
    return new UndefinedTypeVariableInTypeApplicationError(location, name);
  }

  public UndefinedTypeVariableInTypeApplicationError withName(hydra.core.Name name) {
    return new UndefinedTypeVariableInTypeApplicationError(location, name);
  }
}
