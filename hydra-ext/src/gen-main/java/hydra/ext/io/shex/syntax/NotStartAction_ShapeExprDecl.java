// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NotStartAction_ShapeExprDecl implements Serializable, Comparable<NotStartAction_ShapeExprDecl> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl");
  
  public static final hydra.core.Name SHAPE_EXPR_LABEL = new hydra.core.Name("ShapeExprLabel");
  
  public static final hydra.core.Name ALTS = new hydra.core.Name("alts");
  
  public final hydra.ext.io.shex.syntax.ShapeExprLabel ShapeExprLabel;
  
  public final hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts alts;
  
  public NotStartAction_ShapeExprDecl (hydra.ext.io.shex.syntax.ShapeExprLabel ShapeExprLabel, hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts alts) {
    this.ShapeExprLabel = ShapeExprLabel;
    this.alts = alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotStartAction_ShapeExprDecl)) {
      return false;
    }
    NotStartAction_ShapeExprDecl o = (NotStartAction_ShapeExprDecl) other;
    return java.util.Objects.equals(
      this.ShapeExprLabel,
      o.ShapeExprLabel) && java.util.Objects.equals(
      this.alts,
      o.alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(ShapeExprLabel) + 3 * java.util.Objects.hashCode(alts);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NotStartAction_ShapeExprDecl other) {
    int cmp = 0;
    cmp = ((Comparable) ShapeExprLabel).compareTo(other.ShapeExprLabel);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) alts).compareTo(other.alts);
  }
  
  public NotStartAction_ShapeExprDecl withShapeExprLabel(hydra.ext.io.shex.syntax.ShapeExprLabel ShapeExprLabel) {
    return new NotStartAction_ShapeExprDecl(ShapeExprLabel, alts);
  }
  
  public NotStartAction_ShapeExprDecl withAlts(hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts alts) {
    return new NotStartAction_ShapeExprDecl(ShapeExprLabel, alts);
  }
}
