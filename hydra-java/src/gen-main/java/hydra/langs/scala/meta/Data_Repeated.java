package hydra.langs.scala.meta;

public class Data_Repeated {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/scala/meta.Data.Repeated");
  
  public final hydra.langs.scala.meta.Data expr;
  
  public Data_Repeated (hydra.langs.scala.meta.Data expr) {
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