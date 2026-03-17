// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A duplicate field name in a record or union type
 */
public class DuplicateFieldError implements Serializable, Comparable<DuplicateFieldError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.DuplicateFieldError");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The duplicated field name
   */
  public final hydra.core.Name name;

  public DuplicateFieldError (hydra.core.Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DuplicateFieldError)) {
      return false;
    }
    DuplicateFieldError o = (DuplicateFieldError) other;
    return java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DuplicateFieldError other) {
    return ((Comparable) name).compareTo(other.name);
  }
}
