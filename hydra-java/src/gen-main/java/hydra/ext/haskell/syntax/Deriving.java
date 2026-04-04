// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * A 'deriving' statement
 */
public class Deriving implements Serializable, Comparable<Deriving> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.Deriving");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.ext.haskell.syntax.Name> value;

  public Deriving (java.util.List<hydra.ext.haskell.syntax.Name> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Deriving)) {
      return false;
    }
    Deriving o = (Deriving) other;
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
  public int compareTo(Deriving other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
