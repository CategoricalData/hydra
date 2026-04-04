// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * A module name
 */
public class ModuleName implements Serializable, Comparable<ModuleName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.ModuleName");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public ModuleName (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleName)) {
      return false;
    }
    ModuleName o = (ModuleName) other;
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
  public int compareTo(ModuleName other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
