package hydra.ext.shex.syntax;

public abstract class NotStartAction {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.NotStartAction");
  
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
  
  public static final class Start extends hydra.ext.shex.syntax.NotStartAction {
    public final hydra.ext.shex.syntax.NotStartAction_Start value;
    
    public Start (hydra.ext.shex.syntax.NotStartAction_Start value) {
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
  
  public static final class ShapeExprDecl extends hydra.ext.shex.syntax.NotStartAction {
    public final hydra.ext.shex.syntax.NotStartAction_ShapeExprDecl value;
    
    public ShapeExprDecl (hydra.ext.shex.syntax.NotStartAction_ShapeExprDecl value) {
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