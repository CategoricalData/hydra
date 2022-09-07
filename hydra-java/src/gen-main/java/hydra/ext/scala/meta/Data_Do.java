package hydra.ext.scala.meta;

public class Data_Do {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.Do");
  
  public final hydra.ext.scala.meta.Data body;
  
  public final hydra.ext.scala.meta.Data expr;
  
  public Data_Do (hydra.ext.scala.meta.Data body, hydra.ext.scala.meta.Data expr) {
    this.body = body;
    this.expr = expr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Do)) {
      return false;
    }
    Data_Do o = (Data_Do) (other);
    return body.equals(o.body) && expr.equals(o.expr);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * expr.hashCode();
  }
  
  public Data_Do withBody(hydra.ext.scala.meta.Data body) {
    return new Data_Do(body, expr);
  }
  
  public Data_Do withExpr(hydra.ext.scala.meta.Data expr) {
    return new Data_Do(body, expr);
  }
}