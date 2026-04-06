// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A lambda parameter name that violates naming conventions (optional)
 */
public class InvalidLambdaParameterNameError implements Serializable, Comparable<InvalidLambdaParameterNameError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The path to the lambda within the term
   */
  public final hydra.paths.SubtermPath location;

  /**
   * The invalid parameter name
   */
  public final hydra.core.Name name;

  public InvalidLambdaParameterNameError (hydra.paths.SubtermPath location, hydra.core.Name name) {
    this.location = location;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InvalidLambdaParameterNameError)) {
      return false;
    }
    InvalidLambdaParameterNameError o = (InvalidLambdaParameterNameError) other;
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
  public int compareTo(InvalidLambdaParameterNameError other) {
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

  public InvalidLambdaParameterNameError withLocation(hydra.paths.SubtermPath location) {
    return new InvalidLambdaParameterNameError(location, name);
  }

  public InvalidLambdaParameterNameError withName(hydra.core.Name name) {
    return new InvalidLambdaParameterNameError(location, name);
  }
}
