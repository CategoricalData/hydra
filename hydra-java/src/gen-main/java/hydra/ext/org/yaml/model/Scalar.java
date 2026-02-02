// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.yaml.model;

import java.io.Serializable;

/**
 * A union of scalars supported in the YAML failsafe and JSON schemas. Other scalars are not supported here
 */
public abstract class Scalar implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.yaml.model.Scalar");
  
  public static final hydra.core.Name FIELD_NAME_BOOL = new hydra.core.Name("bool");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT = new hydra.core.Name("float");
  
  public static final hydra.core.Name FIELD_NAME_INT = new hydra.core.Name("int");
  
  public static final hydra.core.Name FIELD_NAME_NULL = new hydra.core.Name("null");
  
  public static final hydra.core.Name FIELD_NAME_STR = new hydra.core.Name("str");
  
  private Scalar () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Bool instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(Int instance) ;
    
    R visit(Null instance) ;
    
    R visit(Str instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Scalar instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Bool instance) {
      return otherwise((instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Int instance) {
      return otherwise((instance));
    }
    
    default R visit(Null instance) {
      return otherwise((instance));
    }
    
    default R visit(Str instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Represents a true/false value
   */
  public static final class Bool extends hydra.ext.org.yaml.model.Scalar implements Serializable {
    public final Boolean value;
    
    public Bool (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bool)) {
        return false;
      }
      Bool o = (Bool) (other);
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
  
  /**
   * Represents an approximation to real numbers
   */
  public static final class Float_ extends hydra.ext.org.yaml.model.Scalar implements Serializable {
    public final java.math.BigDecimal value;
    
    public Float_ (java.math.BigDecimal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float_)) {
        return false;
      }
      Float_ o = (Float_) (other);
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
  
  /**
   * Represents arbitrary sized finite mathematical integers
   */
  public static final class Int extends hydra.ext.org.yaml.model.Scalar implements Serializable {
    public final java.math.BigInteger value;
    
    public Int (java.math.BigInteger value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) (other);
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
  
  /**
   * Represents the lack of a value
   */
  public static final class Null extends hydra.ext.org.yaml.model.Scalar implements Serializable {
    public final Boolean value;
    
    public Null (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Null)) {
        return false;
      }
      Null o = (Null) (other);
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
  
  /**
   * A string value
   */
  public static final class Str extends hydra.ext.org.yaml.model.Scalar implements Serializable {
    public final String value;
    
    public Str (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Str)) {
        return false;
      }
      Str o = (Str) (other);
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
