package hydra.ext.scala.meta;

public class Data_Eta {
  public final Data expr;
  
  public Data_Eta (Data expr) {
    this.expr = expr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Eta)) {
      return false;
    }
    Data_Eta o = (Data_Eta) (other);
    return expr.equals(o.expr);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode();
  }
}