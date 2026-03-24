// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A type variable reference to a name that is not bound in scope
 */
public class UndefinedTypeVariableError implements Serializable, Comparable<UndefinedTypeVariableError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.UndefinedTypeVariableError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the undefined type variable
   */
  public final hydra.accessors.AccessorPath location;

  /**
   * The name of the undefined type variable
   */
  public final hydra.core.Name name;

  public UndefinedTypeVariableError (hydra.accessors.AccessorPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UndefinedTypeVariableError)) {
      return false;
    }
    UndefinedTypeVariableError o = (UndefinedTypeVariableError) other;
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
  public int compareTo(UndefinedTypeVariableError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public UndefinedTypeVariableError withLocation(hydra.accessors.AccessorPath location) {
    return new UndefinedTypeVariableError(location, name);
  }

  public UndefinedTypeVariableError withName(hydra.core.Name name) {
    return new UndefinedTypeVariableError(location, name);
  }
}
