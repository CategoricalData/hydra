// Note: this is an automatically generated file. Do not edit.

package hydra.ext.sql.ansi;

import java.io.Serializable;

public abstract class ExactNumericType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/sql/ansi.ExactNumericType");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC = new hydra.core.Name("numeric");
  
  public static final hydra.core.Name FIELD_NAME_DECIMAL = new hydra.core.Name("decimal");
  
  public static final hydra.core.Name FIELD_NAME_DEC = new hydra.core.Name("dec");
  
  public static final hydra.core.Name FIELD_NAME_SMALLINT = new hydra.core.Name("smallint");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public static final hydra.core.Name FIELD_NAME_INT = new hydra.core.Name("int");
  
  public static final hydra.core.Name FIELD_NAME_BIGINT = new hydra.core.Name("bigint");
  
  private ExactNumericType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Numeric instance) ;
    
    R visit(Decimal instance) ;
    
    R visit(Dec instance) ;
    
    R visit(Smallint instance) ;
    
    R visit(Integer_ instance) ;
    
    R visit(Int instance) ;
    
    R visit(Bigint instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExactNumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Numeric instance) {
      return otherwise((instance));
    }
    
    default R visit(Decimal instance) {
      return otherwise((instance));
    }
    
    default R visit(Dec instance) {
      return otherwise((instance));
    }
    
    default R visit(Smallint instance) {
      return otherwise((instance));
    }
    
    default R visit(Integer_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Int instance) {
      return otherwise((instance));
    }
    
    default R visit(Bigint instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Numeric extends hydra.ext.sql.ansi.ExactNumericType implements Serializable {
    public final hydra.util.Opt<hydra.ext.sql.ansi.ExactNumericType_Numeric_Option> value;
    
    public Numeric (hydra.util.Opt<hydra.ext.sql.ansi.ExactNumericType_Numeric_Option> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Numeric)) {
        return false;
      }
      Numeric o = (Numeric) (other);
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
  
  public static final class Decimal extends hydra.ext.sql.ansi.ExactNumericType implements Serializable {
    public final hydra.util.Opt<hydra.ext.sql.ansi.ExactNumericType_Decimal_Option> value;
    
    public Decimal (hydra.util.Opt<hydra.ext.sql.ansi.ExactNumericType_Decimal_Option> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decimal)) {
        return false;
      }
      Decimal o = (Decimal) (other);
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
  
  public static final class Dec extends hydra.ext.sql.ansi.ExactNumericType implements Serializable {
    public final hydra.util.Opt<hydra.ext.sql.ansi.ExactNumericType_Dec_Option> value;
    
    public Dec (hydra.util.Opt<hydra.ext.sql.ansi.ExactNumericType_Dec_Option> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dec)) {
        return false;
      }
      Dec o = (Dec) (other);
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
  
  public static final class Smallint extends hydra.ext.sql.ansi.ExactNumericType implements Serializable {
    public Smallint () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Smallint)) {
        return false;
      }
      Smallint o = (Smallint) (other);
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
  
  public static final class Integer_ extends hydra.ext.sql.ansi.ExactNumericType implements Serializable {
    public Integer_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer_)) {
        return false;
      }
      Integer_ o = (Integer_) (other);
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
  
  public static final class Int extends hydra.ext.sql.ansi.ExactNumericType implements Serializable {
    public Int () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) (other);
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
  
  public static final class Bigint extends hydra.ext.sql.ansi.ExactNumericType implements Serializable {
    public Bigint () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bigint)) {
        return false;
      }
      Bigint o = (Bigint) (other);
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
