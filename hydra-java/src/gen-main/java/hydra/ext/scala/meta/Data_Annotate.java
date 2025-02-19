// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Annotate implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_Annotate");
  
  public static final hydra.core.Name FIELD_NAME_EXPR = new hydra.core.Name("expr");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTS = new hydra.core.Name("annots");
  
  public final hydra.ext.scala.meta.Data expr;
  
  public final java.util.List<hydra.ext.scala.meta.Mod_Annot> annots;
  
  public Data_Annotate (hydra.ext.scala.meta.Data expr, java.util.List<hydra.ext.scala.meta.Mod_Annot> annots) {
    java.util.Objects.requireNonNull((expr));
    java.util.Objects.requireNonNull((annots));
    this.expr = expr;
    this.annots = annots;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Annotate)) {
      return false;
    }
    Data_Annotate o = (Data_Annotate) (other);
    return expr.equals(o.expr) && annots.equals(o.annots);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * annots.hashCode();
  }
  
  public Data_Annotate withExpr(hydra.ext.scala.meta.Data expr) {
    java.util.Objects.requireNonNull((expr));
    return new Data_Annotate(expr, annots);
  }
  
  public Data_Annotate withAnnots(java.util.List<hydra.ext.scala.meta.Mod_Annot> annots) {
    java.util.Objects.requireNonNull((annots));
    return new Data_Annotate(expr, annots);
  }
}