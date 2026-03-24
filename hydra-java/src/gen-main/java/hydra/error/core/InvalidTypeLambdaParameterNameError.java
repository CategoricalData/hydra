// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A type lambda parameter name that violates naming conventions (optional)
 */
public class InvalidTypeLambdaParameterNameError implements Serializable, Comparable<InvalidTypeLambdaParameterNameError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the type lambda within the term
   */
  public final hydra.accessors.AccessorPath location;

  /**
   * The invalid type lambda parameter name
   */
  public final hydra.core.Name name;

  public InvalidTypeLambdaParameterNameError (hydra.accessors.AccessorPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvalidTypeLambdaParameterNameError)) {
      return false;
    }
    InvalidTypeLambdaParameterNameError o = (InvalidTypeLambdaParameterNameError) other;
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
  public int compareTo(InvalidTypeLambdaParameterNameError other) {
    int cmp = 0;
    cmp = ((Comparable) location).compareTo(other.location);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public InvalidTypeLambdaParameterNameError withLocation(hydra.accessors.AccessorPath location) {
    return new InvalidTypeLambdaParameterNameError(location, name);
  }

  public InvalidTypeLambdaParameterNameError withName(hydra.core.Name name) {
    return new InvalidTypeLambdaParameterNameError(location, name);
  }
}
