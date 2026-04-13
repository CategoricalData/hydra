// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_Repeated implements Serializable, Comparable<Type_Repeated> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_Repeated");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public final hydra.scala.syntax.Type tpe;

  public Type_Repeated (hydra.scala.syntax.Type tpe) {
    this.tpe = tpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Repeated)) {
      return false;
    }
    Type_Repeated o = (Type_Repeated) other;
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
  public int compareTo(Type_Repeated other) {
    return hydra.util.Comparing.compare(
      tpe,
      other.tpe);
  }
}
