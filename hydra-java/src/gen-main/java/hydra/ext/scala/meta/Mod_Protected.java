// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Mod_Protected implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Mod_Protected");
  
  public static final hydra.core.Name FIELD_NAME_WITHIN = new hydra.core.Name("within");
  
  public final hydra.ext.scala.meta.Ref within;
  
  public Mod_Protected (hydra.ext.scala.meta.Ref within) {
    java.util.Objects.requireNonNull((within));
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