// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Ascribe implements Serializable, Comparable<Data_Ascribe> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Ascribe");

  public static final hydra.core.Name EXPR = new hydra.core.Name("expr");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public final hydra.ext.scala.syntax.Data expr;

  public final hydra.ext.scala.syntax.Type tpe;

  public Data_Ascribe (hydra.ext.scala.syntax.Data expr, hydra.ext.scala.syntax.Type tpe) {
    this.expr = expr;
    this.tpe = tpe;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Ascribe)) {
      return false;
    }
    Data_Ascribe o = (Data_Ascribe) other;
    return java.util.Objects.equals(
      this.expr,
      o.expr) && java.util.Objects.equals(
      this.tpe,
      o.tpe);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expr) + 3 * java.util.Objects.hashCode(tpe);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Ascribe other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expr,
      other.expr);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      tpe,
      other.tpe);
  }

  public Data_Ascribe withExpr(hydra.ext.scala.syntax.Data expr) {
    return new Data_Ascribe(expr, tpe);
  }

  public Data_Ascribe withTpe(hydra.ext.scala.syntax.Type tpe) {
    return new Data_Ascribe(expr, tpe);
  }
}
