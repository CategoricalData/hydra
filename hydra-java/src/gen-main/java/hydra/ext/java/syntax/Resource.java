package hydra.ext.java.syntax;

public abstract class Resource {
  private Resource () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Local instance) ;
    
    R visit(Variable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Resource instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Local instance) {
      return otherwise((instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Local extends Resource {
    public final Resource_Local value;
    
    public Local (Resource_Local value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Local)) {
        return false;
      }
      Local o = (Local) (other);
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
  
  public static final class Variable extends Resource {
    public final VariableAccess value;
    
    public Variable (VariableAccess value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
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