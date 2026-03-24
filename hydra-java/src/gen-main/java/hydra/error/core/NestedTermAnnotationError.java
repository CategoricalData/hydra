// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A term annotation directly wrapping another term annotation; annotations should be merged (optional)
 */
public class NestedTermAnnotationError implements Serializable, Comparable<NestedTermAnnotationError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.NestedTermAnnotationError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the outer annotation within the term
   */
  public final hydra.paths.SubtermPath location;

  public NestedTermAnnotationError (hydra.paths.SubtermPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NestedTermAnnotationError)) {
      return false;
    }
    NestedTermAnnotationError o = (NestedTermAnnotationError) other;
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
  public int compareTo(NestedTermAnnotationError other) {
    return ((Comparable) location).compareTo(other.location);
  }
}
