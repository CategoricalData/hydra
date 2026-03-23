// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Method implements Serializable, Comparable<Type_Method> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_Method");

  public static final hydra.core.Name PARAMSS = new hydra.core.Name("paramss");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public final hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>> paramss;

  public final hydra.ext.scala.meta.Type tpe;

  public Type_Method (hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>> paramss, hydra.ext.scala.meta.Type tpe) {
    this.paramss = paramss;
    this.tpe = tpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Method)) {
      return false;
    }
    Type_Method o = (Type_Method) other;
    return java.util.Objects.equals(
      this.paramss,
      o.paramss) && java.util.Objects.equals(
      this.tpe,
      o.tpe);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(paramss) + 3 * java.util.Objects.hashCode(tpe);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Method other) {
    int cmp = 0;
    cmp = ((Comparable) paramss).compareTo(other.paramss);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) tpe).compareTo(other.tpe);
  }

  public Type_Method withParamss(hydra.util.ConsList<hydra.util.ConsList<hydra.ext.scala.meta.Data_Param>> paramss) {
    return new Type_Method(paramss, tpe);
  }

  public Type_Method withTpe(hydra.ext.scala.meta.Type tpe) {
    return new Type_Method(paramss, tpe);
  }
}
