// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * An application of an identity lambda to an argument, which simplifies to the argument (optional)
 */
public class UnnecessaryIdentityApplicationError implements Serializable, Comparable<UnnecessaryIdentityApplicationError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the identity application within the term
   */
  public final hydra.paths.SubtermPath location;

  public UnnecessaryIdentityApplicationError (hydra.paths.SubtermPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnnecessaryIdentityApplicationError)) {
      return false;
    }
    UnnecessaryIdentityApplicationError o = (UnnecessaryIdentityApplicationError) other;
    return java.util.Objects.equals(
      this.location,
      o.location);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(location);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnnecessaryIdentityApplicationError other) {
    return hydra.util.Comparing.compare(
      location,
      other.location);
  }
}
