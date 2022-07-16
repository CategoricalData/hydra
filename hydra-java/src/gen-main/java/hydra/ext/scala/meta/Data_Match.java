package hydra.ext.scala.meta;

public class Data_Match {
  public final Data expr;
  
  public final java.util.List<Case> cases;
  
  public Data_Match (Data expr, java.util.List<Case> cases) {
    this.expr = expr;
    this.cases = cases;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Match)) {
      return false;
    }
    Data_Match o = (Data_Match) (other);
    return expr.equals(o.expr) && cases.equals(o.cases);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * cases.hashCode();
  }
  
  public Data_Match withExpr(Data expr) {
    return new Data_Match(expr, cases);
  }
  
  public Data_Match withCases(java.util.List<Case> cases) {
    return new Data_Match(expr, cases);
  }
}