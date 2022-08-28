package hydra.ext.scala.meta;

public class Data_Repeated {
  public final hydra.ext.scala.meta.Data expr;
  
  public Data_Repeated (hydra.ext.scala.meta.Data expr) {
    this.expr = expr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Repeated)) {
      return false;
    }
    Data_Repeated o = (Data_Repeated) (other);
    return expr.equals(o.expr);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode();
  }
}