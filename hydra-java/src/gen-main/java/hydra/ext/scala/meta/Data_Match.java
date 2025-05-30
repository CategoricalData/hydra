// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Match implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_Match");
  
  public static final hydra.core.Name FIELD_NAME_EXPR = new hydra.core.Name("expr");
  
  public static final hydra.core.Name FIELD_NAME_CASES = new hydra.core.Name("cases");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public final java.util.List<hydra.ext.scala.meta.Case> cases;
  
  public Data_Match (hydra.ext.scala.meta.Data expr, java.util.List<hydra.ext.scala.meta.Case> cases) {
    java.util.Objects.requireNonNull((expr));
    java.util.Objects.requireNonNull((cases));
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
  
  public Data_Match withExpr(hydra.ext.scala.meta.Data expr) {
    java.util.Objects.requireNonNull((expr));
    return new Data_Match(expr, cases);
  }
  
  public Data_Match withCases(java.util.List<hydra.ext.scala.meta.Case> cases) {
    java.util.Objects.requireNonNull((cases));
    return new Data_Match(expr, cases);
  }
}