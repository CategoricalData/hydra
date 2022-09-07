package hydra.ext.scala.meta;

public class Data_TryWithHandler {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.TryWithHandler");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public final hydra.ext.scala.meta.Data catchp;
  
  public final java.util.Optional<hydra.ext.scala.meta.Data> finallyp;
  
  public Data_TryWithHandler (hydra.ext.scala.meta.Data expr, hydra.ext.scala.meta.Data catchp, java.util.Optional<hydra.ext.scala.meta.Data> finallyp) {
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
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
  
  public Data_TryWithHandler withCatchp(hydra.ext.scala.meta.Data catchp) {
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
  
  public Data_TryWithHandler withFinallyp(java.util.Optional<hydra.ext.scala.meta.Data> finallyp) {
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
}