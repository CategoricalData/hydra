// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A collection of local bindings
 */
public class LocalBindings implements Serializable, Comparable<LocalBindings> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.LocalBindings");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.haskell.ast.LocalBinding> value;
  
  public LocalBindings (java.util.List<hydra.ext.haskell.ast.LocalBinding> value) {
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
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
