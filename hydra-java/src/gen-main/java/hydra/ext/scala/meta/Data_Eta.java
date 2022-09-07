package hydra.ext.scala.meta;

public class Data_Eta {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.Eta");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public Data_Eta (hydra.ext.scala.meta.Data expr) {
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