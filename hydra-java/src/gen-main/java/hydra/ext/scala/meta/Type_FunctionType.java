package hydra.ext.scala.meta;

public abstract class Type_FunctionType {
  private Type_FunctionType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Function instance) ;
    
    R visit(ContextFunction instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type_FunctionType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Function instance) {
      return otherwise((instance));
    }
    
    default R visit(ContextFunction instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Function extends Type_FunctionType {
    public final Type_Function value;
    
    public Function (Type_Function value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) (other);
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
  
  public static final class ContextFunction extends Type_FunctionType {
    public final Type_ContextFunction value;
    
    public ContextFunction (Type_ContextFunction value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ContextFunction)) {
        return false;
      }
      ContextFunction o = (ContextFunction) (other);
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