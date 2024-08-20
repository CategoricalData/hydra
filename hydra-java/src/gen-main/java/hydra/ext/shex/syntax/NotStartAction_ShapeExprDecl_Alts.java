// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public abstract class NotStartAction_ShapeExprDecl_Alts implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.NotStartAction.ShapeExprDecl.Alts");
  
  public static final hydra.core.Name FIELD_NAME_SHAPE_EXPRESSION = new hydra.core.Name("shapeExpression");
  
  public static final hydra.core.Name FIELD_NAME_E_X_T_E_R_N_A_L = new hydra.core.Name("eXTERNAL");
  
  private NotStartAction_ShapeExprDecl_Alts () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ShapeExpression instance) ;
    
    R visit(EXTERNAL instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NotStartAction_ShapeExprDecl_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ShapeExpression instance) {
      return otherwise((instance));
    }
    
    default R visit(EXTERNAL instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ShapeExpression extends hydra.ext.shex.syntax.NotStartAction_ShapeExprDecl_Alts implements Serializable {
    public final hydra.ext.shex.syntax.ShapeExpression value;
    
    public ShapeExpression (hydra.ext.shex.syntax.ShapeExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShapeExpression)) {
        return false;
      }
      ShapeExpression o = (ShapeExpression) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class EXTERNAL extends hydra.ext.shex.syntax.NotStartAction_ShapeExprDecl_Alts implements Serializable {
    public EXTERNAL () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EXTERNAL)) {
        return false;
      }
      EXTERNAL o = (EXTERNAL) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
