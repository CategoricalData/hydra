// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Do implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_Do");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_EXPR = new hydra.core.Name("expr");
  
  public final hydra.ext.scala.meta.Data body;
  
  public final hydra.ext.scala.meta.Data expr;
  
  public Data_Do (hydra.ext.scala.meta.Data body, hydra.ext.scala.meta.Data expr) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((expr));
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
  
  public Data_Do withBody(hydra.ext.scala.meta.Data body) {
    java.util.Objects.requireNonNull((body));
    return new Data_Do(body, expr);
  }
  
  public Data_Do withExpr(hydra.ext.scala.meta.Data expr) {
    java.util.Objects.requireNonNull((expr));
    return new Data_Do(body, expr);
  }
}