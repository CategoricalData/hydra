// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_PartialFunction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.PartialFunction");
  
  public static final hydra.core.Name FIELD_NAME_CASES = new hydra.core.Name("cases");
  
  public final java.util.List<hydra.ext.scala.meta.Case> cases;
  
  public Data_PartialFunction (java.util.List<hydra.ext.scala.meta.Case> cases) {
    java.util.Objects.requireNonNull((cases));
    this.cases = cases;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_PartialFunction)) {
      return false;
    }
    Data_PartialFunction o = (Data_PartialFunction) (other);
    return cases.equals(o.cases);
  }
  
  @Override
  public int hashCode() {
    return 2 * cases.hashCode();
  }
}
