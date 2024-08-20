// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Importee_Given implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Importee.Given");
  
  public static final hydra.core.Name FIELD_NAME_TPE = new hydra.core.Name("tpe");
  
  public final hydra.ext.scala.meta.Type tpe;
  
  public Importee_Given (hydra.ext.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tpe));
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Given)) {
      return false;
    }
    Importee_Given o = (Importee_Given) (other);
    return tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode();
  }
}
