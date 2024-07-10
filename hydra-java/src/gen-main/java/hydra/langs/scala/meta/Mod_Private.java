// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Mod_Private implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Mod.Private");
  
  public final hydra.langs.scala.meta.Ref within;
  
  public Mod_Private (hydra.langs.scala.meta.Ref within) {
    if (within == null) {
      throw new IllegalArgumentException("null value for 'within' argument");
    }
    this.within = within;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Mod_Private)) {
      return false;
    }
    Mod_Private o = (Mod_Private) (other);
    return within.equals(o.within);
  }
  
  @Override
  public int hashCode() {
    return 2 * within.hashCode();
  }
}