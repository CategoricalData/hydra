// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * Multiple let bindings with the same name were found
 */
public class MultipleBindingsError implements Serializable, Comparable<MultipleBindingsError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.MultipleBindingsError");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The binding name which was duplicated
   */
  public final hydra.core.Name name;

  public MultipleBindingsError (hydra.core.Name name) {
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultipleBindingsError)) {
      return false;
    }
    MultipleBindingsError o = (MultipleBindingsError) other;
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
  public int compareTo(MultipleBindingsError other) {
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }
}
