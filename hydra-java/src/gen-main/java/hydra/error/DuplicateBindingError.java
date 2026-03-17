// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * A duplicate binding name in a let expression
 */
public class DuplicateBindingError implements Serializable, Comparable<DuplicateBindingError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.DuplicateBindingError");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The duplicated binding name
   */
  public final hydra.core.Name name;

  public DuplicateBindingError (hydra.core.Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DuplicateBindingError)) {
      return false;
    }
    DuplicateBindingError o = (DuplicateBindingError) other;
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
  public int compareTo(DuplicateBindingError other) {
    return ((Comparable) name).compareTo(other.name);
  }
}
