package hydra.ext.java.syntax;

public abstract class Result {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Result");
  
  private Result () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Type instance) ;
    
    R visit(Void_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Result instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(Void_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Type extends hydra.ext.java.syntax.Result {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Type");
    
    public final hydra.ext.java.syntax.UnannType value;
    
    public Type (hydra.ext.java.syntax.UnannType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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
  
  public static final class Void_ extends hydra.ext.java.syntax.Result {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Void");
    
    public Void_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Void_)) {
        return false;
      }
      Void_ o = (Void_) (other);
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