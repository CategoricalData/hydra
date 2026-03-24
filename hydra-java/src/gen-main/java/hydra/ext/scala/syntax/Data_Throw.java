// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Throw implements Serializable, Comparable<Data_Throw> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Throw");

  public static final hydra.core.Name EXPR = new hydra.core.Name("expr");

  public final hydra.ext.scala.syntax.Data expr;

  public Data_Throw (hydra.ext.scala.syntax.Data expr) {
    this.expr = expr;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Throw)) {
      return false;
    }
    Data_Throw o = (Data_Throw) other;
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
  public int compareTo(Data_Throw other) {
    return ((Comparable) expr).compareTo(other.expr);
  }
}
