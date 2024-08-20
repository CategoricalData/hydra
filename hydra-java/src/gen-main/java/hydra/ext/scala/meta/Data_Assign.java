// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Assign implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.Assign");
  
  public static final hydra.core.Name FIELD_NAME_LHS = new hydra.core.Name("lhs");
  
  public static final hydra.core.Name FIELD_NAME_RHS = new hydra.core.Name("rhs");
  
  public final hydra.ext.scala.meta.Data lhs;
  
  public final hydra.ext.scala.meta.Data rhs;
  
  public Data_Assign (hydra.ext.scala.meta.Data lhs, hydra.ext.scala.meta.Data rhs) {
    java.util.Objects.requireNonNull((lhs));
    java.util.Objects.requireNonNull((rhs));
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Assign)) {
      return false;
    }
    Data_Assign o = (Data_Assign) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Data_Assign withLhs(hydra.ext.scala.meta.Data lhs) {
    java.util.Objects.requireNonNull((lhs));
    return new Data_Assign(lhs, rhs);
  }
  
  public Data_Assign withRhs(hydra.ext.scala.meta.Data rhs) {
    java.util.Objects.requireNonNull((rhs));
    return new Data_Assign(lhs, rhs);
  }
}
