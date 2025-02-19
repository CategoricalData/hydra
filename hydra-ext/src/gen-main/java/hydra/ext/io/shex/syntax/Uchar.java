// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class Uchar implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE2 = new hydra.core.Name("sequence2");
  
  private Uchar () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sequence instance) ;
    
    R visit(Sequence2 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Uchar instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence2 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Sequence extends hydra.ext.io.shex.syntax.Uchar implements Serializable {
    public final hydra.ext.io.shex.syntax.Uchar_Sequence value;
    
    public Sequence (hydra.ext.io.shex.syntax.Uchar_Sequence value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) (other);
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
  
  public static final class Sequence2 extends hydra.ext.io.shex.syntax.Uchar implements Serializable {
    public final hydra.ext.io.shex.syntax.Uchar_Sequence2 value;
    
    public Sequence2 (hydra.ext.io.shex.syntax.Uchar_Sequence2 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence2)) {
        return false;
      }
      Sequence2 o = (Sequence2) (other);
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