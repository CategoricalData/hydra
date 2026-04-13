// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_Lambda implements Serializable, Comparable<Type_Lambda> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_Lambda");

  public static final hydra.core.Name TPARAMS = new hydra.core.Name("tparams");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public final java.util.List<hydra.scala.syntax.Type_Param> tparams;

  public final hydra.scala.syntax.Type tpe;

  public Type_Lambda (java.util.List<hydra.scala.syntax.Type_Param> tparams, hydra.scala.syntax.Type tpe) {
    this.tparams = tparams;
    this.tpe = tpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Lambda)) {
      return false;
    }
    Type_Lambda o = (Type_Lambda) other;
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
  public int compareTo(Type_Lambda other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      tparams,
      other.tparams);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      tpe,
      other.tpe);
  }

  public Type_Lambda withTparams(java.util.List<hydra.scala.syntax.Type_Param> tparams) {
    return new Type_Lambda(tparams, tpe);
  }

  public Type_Lambda withTpe(hydra.scala.syntax.Type tpe) {
    return new Type_Lambda(tparams, tpe);
  }
}
