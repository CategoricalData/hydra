// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Return implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_Return");
  
  public static final hydra.core.Name FIELD_NAME_EXPR = new hydra.core.Name("expr");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public Data_Return (hydra.ext.scala.meta.Data expr) {
    java.util.Objects.requireNonNull((expr));
    this.expr = expr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Return)) {
      return false;
    }
    Data_Return o = (Data_Return) (other);
    return expr.equals(o.expr);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode();
  }
}