// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class StarPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.StarPattern");
  
  public static final hydra.core.Name FIELD_NAME_CAPTURE = new hydra.core.Name("capture");
  
  public static final hydra.core.Name FIELD_NAME_WILDCARD = new hydra.core.Name("wildcard");
  
  private StarPattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Capture instance) ;
    
    R visit(Wildcard instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StarPattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Capture instance) {
      return otherwise((instance));
    }
    
    default R visit(Wildcard instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Capture extends hydra.ext.python.syntax.StarPattern implements Serializable {
    public final hydra.ext.python.syntax.PatternCaptureTarget value;
    
    public Capture (hydra.ext.python.syntax.PatternCaptureTarget value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Capture)) {
        return false;
      }
      Capture o = (Capture) (other);
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
  
  public static final class Wildcard extends hydra.ext.python.syntax.StarPattern implements Serializable {
    public Wildcard () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) (other);
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