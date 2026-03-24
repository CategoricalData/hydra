// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A type variable in a lambda domain annotation that is not bound in scope
 */
public class UndefinedTypeVariableInLambdaDomainError implements Serializable, Comparable<UndefinedTypeVariableInLambdaDomainError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the lambda within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The name of the undefined type variable
   */
  public final hydra.core.Name name;

  public UndefinedTypeVariableInLambdaDomainError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UndefinedTypeVariableInLambdaDomainError)) {
      return false;
    }
    UndefinedTypeVariableInLambdaDomainError o = (UndefinedTypeVariableInLambdaDomainError) other;
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
  public int compareTo(UndefinedTypeVariableInLambdaDomainError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public UndefinedTypeVariableInLambdaDomainError withLocation(hydra.paths.SubtermPath location) {
    return new UndefinedTypeVariableInLambdaDomainError(location, name);
  }

  public UndefinedTypeVariableInLambdaDomainError withName(hydra.core.Name name) {
    return new UndefinedTypeVariableInLambdaDomainError(location, name);
  }
}
