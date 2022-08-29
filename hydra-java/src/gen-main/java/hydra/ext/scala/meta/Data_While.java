package hydra.ext.scala.meta;

public class Data_While {
  public final hydra.ext.scala.meta.Data expr;
  
  public final hydra.ext.scala.meta.Data body;
  
  public Data_While (hydra.ext.scala.meta.Data expr, hydra.ext.scala.meta.Data body) {
    this.expr = expr;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_While)) {
      return false;
    }
    Data_While o = (Data_While) (other);
    return expr.equals(o.expr) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * body.hashCode();
  }
  
  public Data_While withExpr(hydra.ext.scala.meta.Data expr) {
    return new Data_While(expr, body);
  }
  
  public Data_While withBody(hydra.ext.scala.meta.Data body) {
    return new Data_While(expr, body);
  }
}