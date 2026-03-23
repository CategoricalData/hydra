// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Mod_Annot implements Serializable, Comparable<Mod_Annot> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Mod_Annot");

  public static final hydra.core.Name INIT = new hydra.core.Name("init");

  public final hydra.ext.scala.meta.Init init;

  public Mod_Annot (hydra.ext.scala.meta.Init init) {
    this.init = init;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Mod_Annot)) {
      return false;
    }
    Mod_Annot o = (Mod_Annot) other;
    return java.util.Objects.equals(
      this.init,
      o.init);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(init);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Mod_Annot other) {
    return ((Comparable) init).compareTo(other.init);
  }
}
