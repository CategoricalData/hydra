// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A collection of local bindings
 */
public class LocalBindings implements Serializable, Comparable<LocalBindings> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.LocalBindings");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.haskell.syntax.LocalBinding> value;

  public LocalBindings (java.util.List<hydra.haskell.syntax.LocalBinding> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LocalBindings)) {
      return false;
    }
    LocalBindings o = (LocalBindings) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LocalBindings other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
