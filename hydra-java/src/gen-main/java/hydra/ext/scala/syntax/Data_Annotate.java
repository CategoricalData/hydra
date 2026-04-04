// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Annotate implements Serializable, Comparable<Data_Annotate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Annotate");

  public static final hydra.core.Name EXPR = new hydra.core.Name("expr");

  public static final hydra.core.Name ANNOTS = new hydra.core.Name("annots");

  public final hydra.ext.scala.syntax.Data expr;

  public final java.util.List<hydra.ext.scala.syntax.Mod_Annot> annots;

  public Data_Annotate (hydra.ext.scala.syntax.Data expr, java.util.List<hydra.ext.scala.syntax.Mod_Annot> annots) {
    this.expr = expr;
    this.annots = annots;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Annotate)) {
      return false;
    }
    Data_Annotate o = (Data_Annotate) other;
    return java.util.Objects.equals(
      this.expr,
      o.expr) && java.util.Objects.equals(
      this.annots,
      o.annots);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expr) + 3 * java.util.Objects.hashCode(annots);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Annotate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      expr,
      other.expr);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      annots,
      other.annots);
  }

  public Data_Annotate withExpr(hydra.ext.scala.syntax.Data expr) {
    return new Data_Annotate(expr, annots);
  }

  public Data_Annotate withAnnots(java.util.List<hydra.ext.scala.syntax.Mod_Annot> annots) {
    return new Data_Annotate(expr, annots);
  }
}
