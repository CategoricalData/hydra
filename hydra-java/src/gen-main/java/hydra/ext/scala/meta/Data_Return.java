package hydra.ext.scala.meta;

public class Data_Return {
  public final Data expr;
  
  public Data_Return (Data expr) {
    this.expr = expr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Return)) {
      return false;
    }
    Data_Return o = (Data_Return) (other);
    return expr.equals(o.expr);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode();
  }
}