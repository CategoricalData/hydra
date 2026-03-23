// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Match implements Serializable, Comparable<Data_Match> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Data_Match");

  public static final hydra.core.Name EXPR = new hydra.core.Name("expr");

  public static final hydra.core.Name CASES = new hydra.core.Name("cases");

  public final hydra.ext.scala.meta.Data expr;

  public final hydra.util.ConsList<hydra.ext.scala.meta.Case> cases;

  public Data_Match (hydra.ext.scala.meta.Data expr, hydra.util.ConsList<hydra.ext.scala.meta.Case> cases) {
    this.expr = expr;
    this.cases = cases;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Match)) {
      return false;
    }
    Data_Match o = (Data_Match) other;
    return java.util.Objects.equals(
      this.expr,
      o.expr) && java.util.Objects.equals(
      this.cases,
      o.cases);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expr) + 3 * java.util.Objects.hashCode(cases);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Match other) {
    int cmp = 0;
    cmp = ((Comparable) expr).compareTo(other.expr);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) cases).compareTo(other.cases);
  }

  public Data_Match withExpr(hydra.ext.scala.meta.Data expr) {
    return new Data_Match(expr, cases);
  }

  public Data_Match withCases(hydra.util.ConsList<hydra.ext.scala.meta.Case> cases) {
    return new Data_Match(expr, cases);
  }
}
