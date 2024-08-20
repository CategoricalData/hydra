// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_TryWithHandler implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.TryWithHandler");
  
  public static final hydra.core.Name FIELD_NAME_EXPR = new hydra.core.Name("expr");
  
  public static final hydra.core.Name FIELD_NAME_CATCHP = new hydra.core.Name("catchp");
  
  public static final hydra.core.Name FIELD_NAME_FINALLYP = new hydra.core.Name("finallyp");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public final hydra.ext.scala.meta.Data catchp;
  
  public final hydra.util.Opt<hydra.ext.scala.meta.Data> finallyp;
  
  public Data_TryWithHandler (hydra.ext.scala.meta.Data expr, hydra.ext.scala.meta.Data catchp, hydra.util.Opt<hydra.ext.scala.meta.Data> finallyp) {
    java.util.Objects.requireNonNull((expr));
    java.util.Objects.requireNonNull((catchp));
    java.util.Objects.requireNonNull((finallyp));
    this.expr = expr;
    this.catchp = catchp;
    this.finallyp = finallyp;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_TryWithHandler)) {
      return false;
    }
    Data_TryWithHandler o = (Data_TryWithHandler) (other);
    return expr.equals(o.expr) && catchp.equals(o.catchp) && finallyp.equals(o.finallyp);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * catchp.hashCode() + 5 * finallyp.hashCode();
  }
  
  public Data_TryWithHandler withExpr(hydra.ext.scala.meta.Data expr) {
    java.util.Objects.requireNonNull((expr));
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
  
  public Data_TryWithHandler withCatchp(hydra.ext.scala.meta.Data catchp) {
    java.util.Objects.requireNonNull((catchp));
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
  
  public Data_TryWithHandler withFinallyp(hydra.util.Opt<hydra.ext.scala.meta.Data> finallyp) {
    java.util.Objects.requireNonNull((finallyp));
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
}
