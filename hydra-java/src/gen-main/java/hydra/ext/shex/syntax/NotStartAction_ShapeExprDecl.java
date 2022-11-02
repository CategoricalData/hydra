package hydra.ext.shex.syntax;

public class NotStartAction_ShapeExprDecl {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.NotStartAction.ShapeExprDecl");
  
  public final hydra.ext.shex.syntax.ShapeExprLabel shapeExprLabel;
  
  public final hydra.ext.shex.syntax.NotStartAction_ShapeExprDecl_Alts alts;
  
  public NotStartAction_ShapeExprDecl (hydra.ext.shex.syntax.ShapeExprLabel shapeExprLabel, hydra.ext.shex.syntax.NotStartAction_ShapeExprDecl_Alts alts) {
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
  
  public NotStartAction_ShapeExprDecl withShapeExprLabel(hydra.ext.shex.syntax.ShapeExprLabel shapeExprLabel) {
    return new NotStartAction_ShapeExprDecl(shapeExprLabel, alts);
  }
  
  public NotStartAction_ShapeExprDecl withAlts(hydra.ext.shex.syntax.NotStartAction_ShapeExprDecl_Alts alts) {
    return new NotStartAction_ShapeExprDecl(shapeExprLabel, alts);
  }
}