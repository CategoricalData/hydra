package hydra.ext.scala.meta;

public class Data_SplicedMacroExpr {
  public final hydra.ext.scala.meta.Data body;
  
  public Data_SplicedMacroExpr (hydra.ext.scala.meta.Data body) {
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_SplicedMacroExpr)) {
      return false;
    }
    Data_SplicedMacroExpr o = (Data_SplicedMacroExpr) (other);
    return body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode();
  }
}