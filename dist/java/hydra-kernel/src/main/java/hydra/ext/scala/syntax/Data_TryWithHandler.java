// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_TryWithHandler implements Serializable, Comparable<Data_TryWithHandler> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_TryWithHandler");

  public static final hydra.core.Name EXPR = new hydra.core.Name("expr");

  public static final hydra.core.Name CATCHP = new hydra.core.Name("catchp");

  public static final hydra.core.Name FINALLYP = new hydra.core.Name("finallyp");

  public final hydra.ext.scala.syntax.Data expr;

  public final hydra.ext.scala.syntax.Data catchp;

  public final hydra.util.Maybe<hydra.ext.scala.syntax.Data> finallyp;

  public Data_TryWithHandler (hydra.ext.scala.syntax.Data expr, hydra.ext.scala.syntax.Data catchp, hydra.util.Maybe<hydra.ext.scala.syntax.Data> finallyp) {
    this.expr = expr;
    this.catchp = catchp;
    this.finallyp = finallyp;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_TryWithHandler)) {
      return false;
    }
    Data_TryWithHandler o = (Data_TryWithHandler) other;
    return java.util.Objects.equals(
      this.expr,
      o.expr) && java.util.Objects.equals(
      this.catchp,
      o.catchp) && java.util.Objects.equals(
      this.finallyp,
      o.finallyp);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expr) + 3 * java.util.Objects.hashCode(catchp) + 5 * java.util.Objects.hashCode(finallyp);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_TryWithHandler other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expr,
      other.expr);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      catchp,
      other.catchp);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      finallyp,
      other.finallyp);
  }

  public Data_TryWithHandler withExpr(hydra.ext.scala.syntax.Data expr) {
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }

  public Data_TryWithHandler withCatchp(hydra.ext.scala.syntax.Data catchp) {
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }

  public Data_TryWithHandler withFinallyp(hydra.util.Maybe<hydra.ext.scala.syntax.Data> finallyp) {
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
}
