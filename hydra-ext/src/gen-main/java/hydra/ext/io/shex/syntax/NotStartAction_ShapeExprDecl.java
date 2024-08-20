// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class NotStartAction_ShapeExprDecl implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.NotStartAction.ShapeExprDecl");
  
  public static final hydra.core.Name FIELD_NAME_SHAPE_EXPR_LABEL = new hydra.core.Name("shapeExprLabel");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public final hydra.ext.io.shex.syntax.ShapeExprLabel shapeExprLabel;
  
  public final hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts alts;
  
  public NotStartAction_ShapeExprDecl (hydra.ext.io.shex.syntax.ShapeExprLabel shapeExprLabel, hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts alts) {
    java.util.Objects.requireNonNull((shapeExprLabel));
    java.util.Objects.requireNonNull((alts));
    this.shapeExprLabel = shapeExprLabel;
    this.alts = alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotStartAction_ShapeExprDecl)) {
      return false;
    }
    NotStartAction_ShapeExprDecl o = (NotStartAction_ShapeExprDecl) (other);
    return shapeExprLabel.equals(o.shapeExprLabel) && alts.equals(o.alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * shapeExprLabel.hashCode() + 3 * alts.hashCode();
  }
  
  public NotStartAction_ShapeExprDecl withShapeExprLabel(hydra.ext.io.shex.syntax.ShapeExprLabel shapeExprLabel) {
    java.util.Objects.requireNonNull((shapeExprLabel));
    return new NotStartAction_ShapeExprDecl(shapeExprLabel, alts);
  }
  
  public NotStartAction_ShapeExprDecl withAlts(hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts alts) {
    java.util.Objects.requireNonNull((alts));
    return new NotStartAction_ShapeExprDecl(shapeExprLabel, alts);
  }
}