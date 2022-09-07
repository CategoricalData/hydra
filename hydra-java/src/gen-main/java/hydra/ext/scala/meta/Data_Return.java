package hydra.ext.scala.meta;

public class Data_Return {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.Return");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public Data_Return (hydra.ext.scala.meta.Data expr) {
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