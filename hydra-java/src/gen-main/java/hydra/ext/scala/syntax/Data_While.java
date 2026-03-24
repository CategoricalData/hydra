// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_While implements Serializable, Comparable<Data_While> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_While");

  public static final hydra.core.Name EXPR = new hydra.core.Name("expr");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.ext.scala.syntax.Data expr;

  public final hydra.ext.scala.syntax.Data body;

  public Data_While (hydra.ext.scala.syntax.Data expr, hydra.ext.scala.syntax.Data body) {
    this.expr = expr;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_While)) {
      return false;
    }
    Data_While o = (Data_While) other;
    return java.util.Objects.equals(
      this.expr,
      o.expr) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expr) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_While other) {
    int cmp = 0;
    cmp = ((Comparable) expr).compareTo(other.expr);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }

  public Data_While withExpr(hydra.ext.scala.syntax.Data expr) {
    return new Data_While(expr, body);
  }

  public Data_While withBody(hydra.ext.scala.syntax.Data body) {
    return new Data_While(expr, body);
  }
}
