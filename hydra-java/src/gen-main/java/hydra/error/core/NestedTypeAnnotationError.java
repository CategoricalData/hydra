// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A type annotation directly wrapping another type annotation; annotations should be merged (optional)
 */
public class NestedTypeAnnotationError implements Serializable, Comparable<NestedTypeAnnotationError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.NestedTypeAnnotationError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the outer annotation
   */
  public final hydra.accessors.AccessorPath location;

  public NestedTypeAnnotationError (hydra.accessors.AccessorPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NestedTypeAnnotationError)) {
      return false;
    }
    NestedTypeAnnotationError o = (NestedTypeAnnotationError) other;
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
  public int compareTo(NestedTypeAnnotationError other) {
    return ((Comparable) location).compareTo(other.location);
  }
}
