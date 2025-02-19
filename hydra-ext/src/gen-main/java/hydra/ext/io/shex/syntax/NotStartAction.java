// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class NotStartAction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction");
  
  public static final hydra.core.Name FIELD_NAME_START = new hydra.core.Name("start");
  
  public static final hydra.core.Name FIELD_NAME_SHAPE_EXPR_DECL = new hydra.core.Name("shapeExprDecl");
  
  private NotStartAction () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Start instance) ;
    
    R visit(ShapeExprDecl instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NotStartAction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Start instance) {
      return otherwise((instance));
    }
    
    default R visit(ShapeExprDecl instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Start extends hydra.ext.io.shex.syntax.NotStartAction implements Serializable {
    public final hydra.ext.io.shex.syntax.ShapeExpression value;
    
    public Start (hydra.ext.io.shex.syntax.ShapeExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Start)) {
        return false;
      }
      Start o = (Start) (other);
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
  
  public static final class ShapeExprDecl extends hydra.ext.io.shex.syntax.NotStartAction implements Serializable {
    public final hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl value;
    
    public ShapeExprDecl (hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShapeExprDecl)) {
        return false;
      }
      ShapeExprDecl o = (ShapeExprDecl) (other);
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
}