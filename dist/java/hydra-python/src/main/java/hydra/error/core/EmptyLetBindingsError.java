// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A let expression with an empty list of bindings (optional)
 */
public class EmptyLetBindingsError implements Serializable, Comparable<EmptyLetBindingsError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.EmptyLetBindingsError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the empty let expression within the term
   */
  public final hydra.paths.SubtermPath location;

  public EmptyLetBindingsError (hydra.paths.SubtermPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EmptyLetBindingsError)) {
      return false;
    }
    EmptyLetBindingsError o = (EmptyLetBindingsError) other;
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
  public int compareTo(EmptyLetBindingsError other) {
    return hydra.util.Comparing.compare(
      location,
      other.location);
  }
}
