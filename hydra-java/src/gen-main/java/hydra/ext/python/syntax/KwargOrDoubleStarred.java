// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class KwargOrDoubleStarred implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.KwargOrDoubleStarred");
  
  public static final hydra.core.Name FIELD_NAME_KWARG = new hydra.core.Name("kwarg");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE_STARRED = new hydra.core.Name("doubleStarred");
  
  private KwargOrDoubleStarred () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Kwarg instance) ;
    
    R visit(DoubleStarred instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(KwargOrDoubleStarred instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Kwarg instance) {
      return otherwise((instance));
    }
    
    default R visit(DoubleStarred instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Kwarg extends hydra.ext.python.syntax.KwargOrDoubleStarred implements Serializable {
    public final hydra.ext.python.syntax.Kwarg value;
    
    public Kwarg (hydra.ext.python.syntax.Kwarg value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Kwarg)) {
        return false;
      }
      Kwarg o = (Kwarg) (other);
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
  
  public static final class DoubleStarred extends hydra.ext.python.syntax.KwargOrDoubleStarred implements Serializable {
    public final hydra.ext.python.syntax.Expression value;
    
    public DoubleStarred (hydra.ext.python.syntax.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DoubleStarred)) {
        return false;
      }
      DoubleStarred o = (DoubleStarred) (other);
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