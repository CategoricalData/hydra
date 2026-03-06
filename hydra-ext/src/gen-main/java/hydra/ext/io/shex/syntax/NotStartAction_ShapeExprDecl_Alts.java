// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class NotStartAction_ShapeExprDecl_Alts implements Serializable, Comparable<NotStartAction_ShapeExprDecl_Alts> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts");
  
  public static final hydra.core.Name SHAPE_EXPRESSION = new hydra.core.Name("ShapeExpression");
  
  public static final hydra.core.Name E_X_T_E_R_N_A_L = new hydra.core.Name("EXTERNAL");
  
  private NotStartAction_ShapeExprDecl_Alts () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ShapeExpression instance) ;
    
    R visit(EXTERNAL instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NotStartAction_ShapeExprDecl_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(ShapeExpression instance) {
      return otherwise(instance);
    }
    
    default R visit(EXTERNAL instance) {
      return otherwise(instance);
    }
  }
  
  public static final class ShapeExpression extends hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.ShapeExpression value;
    
    public ShapeExpression (hydra.ext.io.shex.syntax.ShapeExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShapeExpression)) {
        return false;
      }
      ShapeExpression o = (ShapeExpression) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NotStartAction_ShapeExprDecl_Alts other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ShapeExpression o = (ShapeExpression) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class EXTERNAL extends hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts implements Serializable {
    public EXTERNAL () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EXTERNAL)) {
        return false;
      }
      EXTERNAL o = (EXTERNAL) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NotStartAction_ShapeExprDecl_Alts other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
