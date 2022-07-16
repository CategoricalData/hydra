package hydra.ext.scala.meta;

public class Data_Throw {
  public final Data expr;
  
  public Data_Throw (Data expr) {
    this.expr = expr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Throw)) {
      return false;
    }
    Data_Throw o = (Data_Throw) (other);
    return expr.equals(o.expr);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode();
  }
}