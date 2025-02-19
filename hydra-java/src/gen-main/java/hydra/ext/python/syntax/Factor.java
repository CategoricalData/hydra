// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class Factor implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Factor");
  
  public static final hydra.core.Name FIELD_NAME_POSITIVE = new hydra.core.Name("positive");
  
  public static final hydra.core.Name FIELD_NAME_NEGATIVE = new hydra.core.Name("negative");
  
  public static final hydra.core.Name FIELD_NAME_COMPLEMENT = new hydra.core.Name("complement");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  private Factor () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Positive instance) ;
    
    R visit(Negative instance) ;
    
    R visit(Complement instance) ;
    
    R visit(Simple instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Factor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Positive instance) {
      return otherwise((instance));
    }
    
    default R visit(Negative instance) {
      return otherwise((instance));
    }
    
    default R visit(Complement instance) {
      return otherwise((instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Positive extends hydra.ext.python.syntax.Factor implements Serializable {
    public final hydra.ext.python.syntax.Factor value;
    
    public Positive (hydra.ext.python.syntax.Factor value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Positive)) {
        return false;
      }
      Positive o = (Positive) (other);
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
  
  public static final class Negative extends hydra.ext.python.syntax.Factor implements Serializable {
    public final hydra.ext.python.syntax.Factor value;
    
    public Negative (hydra.ext.python.syntax.Factor value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Negative)) {
        return false;
      }
      Negative o = (Negative) (other);
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
  
  public static final class Complement extends hydra.ext.python.syntax.Factor implements Serializable {
    public final hydra.ext.python.syntax.Factor value;
    
    public Complement (hydra.ext.python.syntax.Factor value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Complement)) {
        return false;
      }
      Complement o = (Complement) (other);
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
  
  public static final class Simple extends hydra.ext.python.syntax.Factor implements Serializable {
    public final hydra.ext.python.syntax.Power value;
    
    public Simple (hydra.ext.python.syntax.Power value) {
      java.util.Objects.requireNonNull((value));
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
}