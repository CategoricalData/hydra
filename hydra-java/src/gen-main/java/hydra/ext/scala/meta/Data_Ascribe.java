// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Ascribe implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.Ascribe");
  
  public static final hydra.core.Name FIELD_NAME_EXPR = new hydra.core.Name("expr");
  
  public static final hydra.core.Name FIELD_NAME_TPE = new hydra.core.Name("tpe");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public final hydra.ext.scala.meta.Type tpe;
  
  public Data_Ascribe (hydra.ext.scala.meta.Data expr, hydra.ext.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((expr));
    java.util.Objects.requireNonNull((tpe));
    this.expr = expr;
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Ascribe)) {
      return false;
    }
    Data_Ascribe o = (Data_Ascribe) (other);
    return expr.equals(o.expr) && tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * tpe.hashCode();
  }
  
  public Data_Ascribe withExpr(hydra.ext.scala.meta.Data expr) {
    java.util.Objects.requireNonNull((expr));
    return new Data_Ascribe(expr, tpe);
  }
  
  public Data_Ascribe withTpe(hydra.ext.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tpe));
    return new Data_Ascribe(expr, tpe);
  }
}
