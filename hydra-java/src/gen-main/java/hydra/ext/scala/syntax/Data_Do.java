// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Do implements Serializable, Comparable<Data_Do> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Do");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name EXPR = new hydra.core.Name("expr");

  public final hydra.ext.scala.syntax.Data body;

  public final hydra.ext.scala.syntax.Data expr;

  public Data_Do (hydra.ext.scala.syntax.Data body, hydra.ext.scala.syntax.Data expr) {
    this.body = body;
    this.expr = expr;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Do)) {
      return false;
    }
    Data_Do o = (Data_Do) other;
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.expr,
      o.expr);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(expr);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Do other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expr,
      other.expr);
  }

  public Data_Do withBody(hydra.ext.scala.syntax.Data body) {
    return new Data_Do(body, expr);
  }

  public Data_Do withExpr(hydra.ext.scala.syntax.Data expr) {
    return new Data_Do(body, expr);
  }
}
