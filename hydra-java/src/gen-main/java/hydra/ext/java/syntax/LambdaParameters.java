package hydra.ext.java.syntax;

public abstract class LambdaParameters {
  private LambdaParameters () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Tuple instance) ;
    
    R visit(Single instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LambdaParameters instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(Single instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Tuple extends hydra.ext.java.syntax.LambdaParameters {
    public final java.util.List<hydra.ext.java.syntax.LambdaParameters> value;
    
    public Tuple (java.util.List<hydra.ext.java.syntax.LambdaParameters> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) (other);
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
  
  public static final class Single extends hydra.ext.java.syntax.LambdaParameters {
    public final hydra.ext.java.syntax.Identifier value;
    
    public Single (hydra.ext.java.syntax.Identifier value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Single)) {
        return false;
      }
      Single o = (Single) (other);
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