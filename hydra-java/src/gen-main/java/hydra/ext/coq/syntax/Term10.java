package hydra.ext.coq.syntax;

public abstract class Term10 {
  private Term10 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Application instance) ;
    
    R visit(OneTerm instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term10 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Application instance) {
      return otherwise((instance));
    }
    
    default R visit(OneTerm instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Application extends Term10 {
    public final Application value;
    
    public Application (Application value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) (other);
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
  
  public static final class OneTerm extends Term10 {
    public final OneTerm value;
    
    public OneTerm (OneTerm value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OneTerm)) {
        return false;
      }
      OneTerm o = (OneTerm) (other);
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