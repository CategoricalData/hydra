package hydra.ext.scala.meta;

public class Data_TryWithHandler {
  public final Data expr;
  
  public final Data catchp;
  
  public final java.util.Optional<Data> finallyp;
  
  public Data_TryWithHandler (Data expr, Data catchp, java.util.Optional<Data> finallyp) {
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
  
  public Data_TryWithHandler withExpr(Data expr) {
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
  
  public Data_TryWithHandler withCatchp(Data catchp) {
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
  
  public Data_TryWithHandler withFinallyp(java.util.Optional<Data> finallyp) {
    return new Data_TryWithHandler(expr, catchp, finallyp);
  }
}