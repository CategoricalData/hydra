// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Try implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_Try");
  
  public static final hydra.core.Name FIELD_NAME_EXPR = new hydra.core.Name("expr");
  
  public static final hydra.core.Name FIELD_NAME_CATCHP = new hydra.core.Name("catchp");
  
  public static final hydra.core.Name FIELD_NAME_FINALLYP = new hydra.core.Name("finallyp");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public final java.util.List<hydra.ext.scala.meta.Case> catchp;
  
  public final hydra.util.Opt<hydra.ext.scala.meta.Data> finallyp;
  
  public Data_Try (hydra.ext.scala.meta.Data expr, java.util.List<hydra.ext.scala.meta.Case> catchp, hydra.util.Opt<hydra.ext.scala.meta.Data> finallyp) {
    java.util.Objects.requireNonNull((expr));
    java.util.Objects.requireNonNull((catchp));
    java.util.Objects.requireNonNull((finallyp));
    this.expr = expr;
    this.catchp = catchp;
    this.finallyp = finallyp;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Try)) {
      return false;
    }
    Data_Try o = (Data_Try) (other);
    return expr.equals(o.expr) && catchp.equals(o.catchp) && finallyp.equals(o.finallyp);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * catchp.hashCode() + 5 * finallyp.hashCode();
  }
  
  public Data_Try withExpr(hydra.ext.scala.meta.Data expr) {
    java.util.Objects.requireNonNull((expr));
    return new Data_Try(expr, catchp, finallyp);
  }
  
  public Data_Try withCatchp(java.util.List<hydra.ext.scala.meta.Case> catchp) {
    java.util.Objects.requireNonNull((catchp));
    return new Data_Try(expr, catchp, finallyp);
  }
  
  public Data_Try withFinallyp(hydra.util.Opt<hydra.ext.scala.meta.Data> finallyp) {
    java.util.Objects.requireNonNull((finallyp));
    return new Data_Try(expr, catchp, finallyp);
  }
}