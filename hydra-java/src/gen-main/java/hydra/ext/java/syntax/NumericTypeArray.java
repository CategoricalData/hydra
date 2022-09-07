package hydra.ext.java.syntax;

public abstract class NumericTypeArray {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.NumericTypeArray");
  
  private NumericTypeArray () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Simple instance) ;
    
    R visit(Array instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericTypeArray instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Simple extends hydra.ext.java.syntax.NumericTypeArray {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Simple");
    
    public final hydra.ext.java.syntax.NumericType value;
    
    public Simple (hydra.ext.java.syntax.NumericType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
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
  
  public static final class Array extends hydra.ext.java.syntax.NumericTypeArray {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Array");
    
    public final hydra.ext.java.syntax.NumericTypeArray value;
    
    public Array (hydra.ext.java.syntax.NumericTypeArray value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) (other);
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