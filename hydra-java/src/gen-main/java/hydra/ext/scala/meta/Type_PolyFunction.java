// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_PolyFunction implements Serializable, Comparable<Type_PolyFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_PolyFunction");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Type_Param> tparams;

  public final hydra.ext.scala.meta.Type tpe;

  public Type_PolyFunction (hydra.util.ConsList<hydra.ext.scala.meta.Type_Param> tparams, hydra.ext.scala.meta.Type tpe) {
    this.tparams = tparams;
    this.tpe = tpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_PolyFunction)) {
      return false;
    }
    Type_PolyFunction o = (Type_PolyFunction) other;
    return java.util.Objects.equals(
      this.tparams,
      o.tparams) && java.util.Objects.equals(
      this.tpe,
      o.tpe);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tparams) + 3 * java.util.Objects.hashCode(tpe);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_PolyFunction other) {
    int cmp = 0;
    cmp = ((Comparable) tparams).compareTo(other.tparams);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) tpe).compareTo(other.tpe);
  }

  public Type_PolyFunction withTparams(hydra.util.ConsList<hydra.ext.scala.meta.Type_Param> tparams) {
    return new Type_PolyFunction(tparams, tpe);
  }

  public Type_PolyFunction withTpe(hydra.ext.scala.meta.Type tpe) {
    return new Type_PolyFunction(tparams, tpe);
  }
}
