package hydra.ext.java.syntax;

public abstract class TypeArgumentsOrDiamond {
  private TypeArgumentsOrDiamond () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Arguments instance) ;
    
    R visit(Diamond instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeArgumentsOrDiamond instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Arguments instance) {
      return otherwise((instance));
    }
    
    default R visit(Diamond instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Arguments extends TypeArgumentsOrDiamond {
    public final java.util.List<TypeArgument> value;
    
    public Arguments (java.util.List<TypeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Arguments)) {
        return false;
      }
      Arguments o = (Arguments) (other);
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
  
  public static final class Diamond extends TypeArgumentsOrDiamond {
    public Diamond () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Diamond)) {
        return false;
      }
      Diamond o = (Diamond) (other);
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