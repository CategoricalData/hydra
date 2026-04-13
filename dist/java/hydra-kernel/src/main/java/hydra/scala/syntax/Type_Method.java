// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_Method implements Serializable, Comparable<Type_Method> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_Method");

  public static final hydra.core.Name PARAMSS = new hydra.core.Name("paramss");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public final java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss;

  public final hydra.scala.syntax.Type tpe;

  public Type_Method (java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss, hydra.scala.syntax.Type tpe) {
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
    cmp = hydra.util.Comparing.compare(
      paramss,
      other.paramss);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      tpe,
      other.tpe);
  }

  public Type_Method withParamss(java.util.List<java.util.List<hydra.scala.syntax.Data_Param>> paramss) {
    return new Type_Method(paramss, tpe);
  }

  public Type_Method withTpe(hydra.scala.syntax.Type tpe) {
    return new Type_Method(paramss, tpe);
  }
}
