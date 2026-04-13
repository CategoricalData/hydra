// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Data_PartialFunction implements Serializable, Comparable<Data_PartialFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data_PartialFunction");

  public static final hydra.core.Name CASES = new hydra.core.Name("cases");

  public final java.util.List<hydra.scala.syntax.Case> cases;

  public Data_PartialFunction (java.util.List<hydra.scala.syntax.Case> cases) {
    this.cases = cases;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_PartialFunction)) {
      return false;
    }
    Data_PartialFunction o = (Data_PartialFunction) other;
    return java.util.Objects.equals(
      this.cases,
      o.cases);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cases);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_PartialFunction other) {
    return hydra.util.Comparing.compare(
      cases,
      other.cases);
  }
}
