// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A term annotation with an empty annotation map (optional)
 */
public class EmptyTermAnnotationError implements Serializable, Comparable<EmptyTermAnnotationError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.EmptyTermAnnotationError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the empty annotation within the term
   */
  public final hydra.paths.SubtermPath location;

  public EmptyTermAnnotationError (hydra.paths.SubtermPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EmptyTermAnnotationError)) {
      return false;
    }
    EmptyTermAnnotationError o = (EmptyTermAnnotationError) other;
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
  public int compareTo(EmptyTermAnnotationError other) {
    return ((Comparable) location).compareTo(other.location);
  }
}
