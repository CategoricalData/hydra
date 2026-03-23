// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Importee_Given implements Serializable, Comparable<Importee_Given> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Importee_Given");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public final hydra.ext.scala.meta.Type tpe;

  public Importee_Given (hydra.ext.scala.meta.Type tpe) {
    this.tpe = tpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Importee_Given)) {
      return false;
    }
    Importee_Given o = (Importee_Given) other;
    return java.util.Objects.equals(
      this.tpe,
      o.tpe);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tpe);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Importee_Given other) {
    return ((Comparable) tpe).compareTo(other.tpe);
  }
}
