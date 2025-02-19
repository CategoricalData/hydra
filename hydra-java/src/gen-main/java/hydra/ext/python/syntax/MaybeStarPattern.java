// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class MaybeStarPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.MaybeStarPattern");
  
  public static final hydra.core.Name FIELD_NAME_STAR = new hydra.core.Name("star");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  private MaybeStarPattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Star instance) ;
    
    R visit(Pattern instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MaybeStarPattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Star instance) {
      return otherwise((instance));
    }
    
    default R visit(Pattern instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Star extends hydra.ext.python.syntax.MaybeStarPattern implements Serializable {
    public final hydra.ext.python.syntax.StarPattern value;
    
    public Star (hydra.ext.python.syntax.StarPattern value) {
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
  
  public static final class Pattern extends hydra.ext.python.syntax.MaybeStarPattern implements Serializable {
    public final hydra.ext.python.syntax.Pattern value;
    
    public Pattern (hydra.ext.python.syntax.Pattern value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) (other);
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