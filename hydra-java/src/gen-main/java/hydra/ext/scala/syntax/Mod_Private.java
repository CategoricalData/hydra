// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Mod_Private implements Serializable, Comparable<Mod_Private> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Mod_Private");

  public static final hydra.core.Name WITHIN = new hydra.core.Name("within");

  public final hydra.ext.scala.syntax.Ref within;

  public Mod_Private (hydra.ext.scala.syntax.Ref within) {
    this.within = within;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Mod_Private)) {
      return false;
    }
    Mod_Private o = (Mod_Private) other;
    return java.util.Objects.equals(
      this.within,
      o.within);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(within);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Mod_Private other) {
    return ((Comparable) within).compareTo(other.within);
  }
}
