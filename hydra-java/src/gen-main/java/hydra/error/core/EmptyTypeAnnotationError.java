// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A type annotation with an empty annotation map (optional)
 */
public class EmptyTypeAnnotationError implements Serializable, Comparable<EmptyTypeAnnotationError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the empty annotation
   */
  public final hydra.paths.SubtermPath location;

  public EmptyTypeAnnotationError (hydra.paths.SubtermPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EmptyTypeAnnotationError)) {
      return false;
    }
    EmptyTypeAnnotationError o = (EmptyTypeAnnotationError) other;
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
  public int compareTo(EmptyTypeAnnotationError other) {
    return hydra.util.Comparing.compare(
      location,
      other.location);
  }
}
