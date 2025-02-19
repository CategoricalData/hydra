// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Enumerator_Guard implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Enumerator_Guard");
  
  public static final hydra.core.Name FIELD_NAME_COND = new hydra.core.Name("cond");
  
  public final hydra.ext.scala.meta.Data cond;
  
  public Enumerator_Guard (hydra.ext.scala.meta.Data cond) {
    java.util.Objects.requireNonNull((cond));
    this.cond = cond;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Enumerator_Guard)) {
      return false;
    }
    Enumerator_Guard o = (Enumerator_Guard) (other);
    return cond.equals(o.cond);
  }
  
  @Override
  public int hashCode() {
    return 2 * cond.hashCode();
  }
}