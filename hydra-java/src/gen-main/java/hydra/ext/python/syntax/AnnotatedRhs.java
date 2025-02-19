// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class AnnotatedRhs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AnnotatedRhs");
  
  public static final hydra.core.Name FIELD_NAME_YIELD = new hydra.core.Name("yield");
  
  public static final hydra.core.Name FIELD_NAME_STAR = new hydra.core.Name("star");
  
  private AnnotatedRhs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Yield instance) ;
    
    R visit(Star instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AnnotatedRhs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Yield instance) {
      return otherwise((instance));
    }
    
    default R visit(Star instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Yield extends hydra.ext.python.syntax.AnnotatedRhs implements Serializable {
    public final hydra.ext.python.syntax.YieldExpression value;
    
    public Yield (hydra.ext.python.syntax.YieldExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Yield)) {
        return false;
      }
      Yield o = (Yield) (other);
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
  
  public static final class Star extends hydra.ext.python.syntax.AnnotatedRhs implements Serializable {
    public final java.util.List<hydra.ext.python.syntax.StarExpression> value;
    
    public Star (java.util.List<hydra.ext.python.syntax.StarExpression> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Star)) {
        return false;
      }
      Star o = (Star) (other);
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