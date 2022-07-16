package hydra.ext.java.syntax;

public abstract class ForStatement {
  private ForStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Basic instance) ;
    
    R visit(Enhanced instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ForStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Basic instance) {
      return otherwise((instance));
    }
    
    default R visit(Enhanced instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Basic extends ForStatement {
    public final BasicForStatement value;
    
    public Basic (BasicForStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Basic)) {
        return false;
      }
      Basic o = (Basic) (other);
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
  
  public static final class Enhanced extends ForStatement {
    public final EnhancedForStatement value;
    
    public Enhanced (EnhancedForStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enhanced)) {
        return false;
      }
      Enhanced o = (Enhanced) (other);
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