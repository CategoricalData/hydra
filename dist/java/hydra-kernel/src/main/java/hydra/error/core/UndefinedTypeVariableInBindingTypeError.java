// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A type variable in a let binding's type scheme that is not bound by the scheme or enclosing scope
 */
public class UndefinedTypeVariableInBindingTypeError implements Serializable, Comparable<UndefinedTypeVariableInBindingTypeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the binding within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The name of the undefined type variable
   */
  public final hydra.core.Name name;

  public UndefinedTypeVariableInBindingTypeError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UndefinedTypeVariableInBindingTypeError)) {
      return false;
    }
    UndefinedTypeVariableInBindingTypeError o = (UndefinedTypeVariableInBindingTypeError) other;
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
  public int compareTo(UndefinedTypeVariableInBindingTypeError other) {
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

  public UndefinedTypeVariableInBindingTypeError withLocation(hydra.paths.SubtermPath location) {
    return new UndefinedTypeVariableInBindingTypeError(location, name);
  }

  public UndefinedTypeVariableInBindingTypeError withName(hydra.core.Name name) {
    return new UndefinedTypeVariableInBindingTypeError(location, name);
  }
}
