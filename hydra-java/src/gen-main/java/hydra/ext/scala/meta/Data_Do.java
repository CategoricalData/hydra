package hydra.ext.scala.meta;

public class Data_Do {
  public final Data body;
  
  public final Data expr;
  
  public Data_Do (Data body, Data expr) {
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
  
  public Data_Do withBody(Data body) {
    return new Data_Do(body, expr);
  }
  
  public Data_Do withExpr(Data expr) {
    return new Data_Do(body, expr);
  }
}