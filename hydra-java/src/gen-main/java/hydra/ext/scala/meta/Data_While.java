// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_While implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.While");
  
  public static final hydra.core.Name FIELD_NAME_EXPR = new hydra.core.Name("expr");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public final hydra.ext.scala.meta.Data body;
  
  public Data_While (hydra.ext.scala.meta.Data expr, hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((expr));
    java.util.Objects.requireNonNull((body));
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
    java.util.Objects.requireNonNull((expr));
    return new Data_While(expr, body);
  }
  
  public Data_While withBody(hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((body));
    return new Data_While(expr, body);
  }
}
