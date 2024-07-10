// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Mod_Protected implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Mod.Protected");
  
  public final hydra.langs.scala.meta.Ref within;
  
  public Mod_Protected (hydra.langs.scala.meta.Ref within) {
    if (within == null) {
      throw new IllegalArgumentException("null value for 'within' argument");
    }
    this.within = within;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Mod_Protected)) {
      return false;
    }
    Mod_Protected o = (Mod_Protected) (other);
    return within.equals(o.within);
  }
  
  @Override
  public int hashCode() {
    return 2 * within.hashCode();
  }
}