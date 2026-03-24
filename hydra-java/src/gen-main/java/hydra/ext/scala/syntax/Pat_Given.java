// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Pat_Given implements Serializable, Comparable<Pat_Given> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Pat_Given");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public final hydra.ext.scala.syntax.Type tpe;

  public Pat_Given (hydra.ext.scala.syntax.Type tpe) {
    this.tpe = tpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Given)) {
      return false;
    }
    Pat_Given o = (Pat_Given) other;
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
  public int compareTo(Pat_Given other) {
    return ((Comparable) tpe).compareTo(other.tpe);
  }
}
