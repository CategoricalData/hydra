package hydra.ext.scala.meta;

public class Data_Try {
  public final Data expr;
  
  public final java.util.List<Case> catchp;
  
  public final java.util.Optional<Data> finallyp;
  
  public Data_Try (Data expr, java.util.List<Case> catchp, java.util.Optional<Data> finallyp) {
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
  
  public Data_Try withExpr(Data expr) {
    return new Data_Try(expr, catchp, finallyp);
  }
  
  public Data_Try withCatchp(java.util.List<Case> catchp) {
    return new Data_Try(expr, catchp, finallyp);
  }
  
  public Data_Try withFinallyp(java.util.Optional<Data> finallyp) {
    return new Data_Try(expr, catchp, finallyp);
  }
}