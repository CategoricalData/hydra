// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class Slice implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Slice");
  
  public static final hydra.core.Name FIELD_NAME_NAMED = new hydra.core.Name("named");
  
  public static final hydra.core.Name FIELD_NAME_SLICE = new hydra.core.Name("slice");
  
  private Slice () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Named instance) ;
    
    R visit(Slice_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Slice instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Named instance) {
      return otherwise((instance));
    }
    
    default R visit(Slice_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Named extends hydra.ext.python.syntax.Slice implements Serializable {
    public final hydra.ext.python.syntax.NamedExpression value;
    
    public Named (hydra.ext.python.syntax.NamedExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) (other);
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
  
  public static final class Slice_ extends hydra.ext.python.syntax.Slice implements Serializable {
    public final hydra.ext.python.syntax.SliceExpression value;
    
    public Slice_ (hydra.ext.python.syntax.SliceExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Slice_)) {
        return false;
      }
      Slice_ o = (Slice_) (other);
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