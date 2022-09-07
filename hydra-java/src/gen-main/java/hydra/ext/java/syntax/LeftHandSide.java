package hydra.ext.java.syntax;

public abstract class LeftHandSide {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.LeftHandSide");
  
  private LeftHandSide () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ExpressionName instance) ;
    
    R visit(FieldAccess instance) ;
    
    R visit(ArrayAccess instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LeftHandSide instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ExpressionName instance) {
      return otherwise((instance));
    }
    
    default R visit(FieldAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(ArrayAccess instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ExpressionName extends hydra.ext.java.syntax.LeftHandSide {
    public final hydra.ext.java.syntax.ExpressionName value;
    
    public ExpressionName (hydra.ext.java.syntax.ExpressionName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExpressionName)) {
        return false;
      }
      ExpressionName o = (ExpressionName) (other);
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
  
  public static final class FieldAccess extends hydra.ext.java.syntax.LeftHandSide {
    public final hydra.ext.java.syntax.FieldAccess value;
    
    public FieldAccess (hydra.ext.java.syntax.FieldAccess value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FieldAccess)) {
        return false;
      }
      FieldAccess o = (FieldAccess) (other);
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
  
  public static final class ArrayAccess extends hydra.ext.java.syntax.LeftHandSide {
    public final hydra.ext.java.syntax.ArrayAccess value;
    
    public ArrayAccess (hydra.ext.java.syntax.ArrayAccess value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ArrayAccess)) {
        return false;
      }
      ArrayAccess o = (ArrayAccess) (other);
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