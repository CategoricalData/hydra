package hydra.ext.java.syntax;

public abstract class ExplicitConstructorInvocation_Variant {
  private ExplicitConstructorInvocation_Variant () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(This instance) ;
    
    R visit(Super instance) ;
    
    R visit(Primary instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExplicitConstructorInvocation_Variant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(This instance) {
      return otherwise((instance));
    }
    
    default R visit(Super instance) {
      return otherwise((instance));
    }
    
    default R visit(Primary instance) {
      return otherwise((instance));
    }
  }
  
  public static final class This extends hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant {
    public This () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof This)) {
        return false;
      }
      This o = (This) (other);
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
  
  public static final class Super extends hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant {
    public final java.util.Optional<hydra.ext.java.syntax.ExpressionName> value;
    
    public Super (java.util.Optional<hydra.ext.java.syntax.ExpressionName> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Super)) {
        return false;
      }
      Super o = (Super) (other);
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
  
  public static final class Primary extends hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant {
    public final hydra.ext.java.syntax.Primary value;
    
    public Primary (hydra.ext.java.syntax.Primary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) (other);
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