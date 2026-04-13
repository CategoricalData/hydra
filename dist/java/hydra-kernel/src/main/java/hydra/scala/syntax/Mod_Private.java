// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Mod_Private implements Serializable, Comparable<Mod_Private> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Mod_Private");

  public static final hydra.core.Name WITHIN = new hydra.core.Name("within");

  public final hydra.scala.syntax.Ref within;

  public Mod_Private (hydra.scala.syntax.Ref within) {
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
    return hydra.util.Comparing.compare(
      within,
      other.within);
  }
}
