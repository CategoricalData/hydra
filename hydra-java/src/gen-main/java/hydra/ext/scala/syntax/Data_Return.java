// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Return implements Serializable, Comparable<Data_Return> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Return");

  public static final hydra.core.Name EXPR = new hydra.core.Name("expr");

  public final hydra.ext.scala.syntax.Data expr;

  public Data_Return (hydra.ext.scala.syntax.Data expr) {
    this.expr = expr;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Return)) {
      return false;
    }
    Data_Return o = (Data_Return) other;
    return java.util.Objects.equals(
      this.expr,
      o.expr);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expr);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Return other) {
    return ((Comparable) expr).compareTo(other.expr);
  }
}
