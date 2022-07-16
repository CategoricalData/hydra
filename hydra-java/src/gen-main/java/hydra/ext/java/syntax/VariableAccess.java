package hydra.ext.java.syntax;

public abstract class VariableAccess {
  private VariableAccess () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ExpressionName instance) ;
    
    R visit(FieldAccess instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(VariableAccess instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ExpressionName instance) {
      return otherwise((instance));
    }
    
    default R visit(FieldAccess instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ExpressionName extends VariableAccess {
    public final ExpressionName value;
    
    public ExpressionName (ExpressionName value) {
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
  
  public static final class FieldAccess extends VariableAccess {
    public final FieldAccess value;
    
    public FieldAccess (FieldAccess value) {
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
}