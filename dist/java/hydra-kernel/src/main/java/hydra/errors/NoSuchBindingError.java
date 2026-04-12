// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * No let binding with the expected name was present
 */
public class NoSuchBindingError implements Serializable, Comparable<NoSuchBindingError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.NoSuchBindingError");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The binding name which was not found
   */
  public final hydra.core.Name name;

  public NoSuchBindingError (hydra.core.Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NoSuchBindingError)) {
      return false;
    }
    NoSuchBindingError o = (NoSuchBindingError) other;
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
  public int compareTo(NoSuchBindingError other) {
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }
}
