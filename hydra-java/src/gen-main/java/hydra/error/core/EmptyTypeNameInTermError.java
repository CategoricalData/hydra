// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * A record, injection, projection, or case statement with an empty type name (optional)
 */
public class EmptyTypeNameInTermError implements Serializable, Comparable<EmptyTypeNameInTermError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError");

  public static final hydra.core.Name LOCATION = new hydra.core.Name("location");

  /**
   * The path to the term with the empty type name
   */
  public final hydra.paths.SubtermPath location;

  public EmptyTypeNameInTermError (hydra.paths.SubtermPath location) {
    this.location = location;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EmptyTypeNameInTermError)) {
      return false;
    }
    EmptyTypeNameInTermError o = (EmptyTypeNameInTermError) other;
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
  public int compareTo(EmptyTypeNameInTermError other) {
    return hydra.util.Comparing.compare(
      location,
      other.location);
  }
}
