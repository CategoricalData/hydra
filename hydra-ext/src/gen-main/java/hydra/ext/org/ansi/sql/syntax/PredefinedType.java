// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class PredefinedType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.PredefinedType");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_NATIONAL_STRING = new hydra.core.Name("nationalString");
  
  public static final hydra.core.Name FIELD_NAME_BLOB = new hydra.core.Name("blob");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC = new hydra.core.Name("numeric");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_DATETIME = new hydra.core.Name("datetime");
  
  public static final hydra.core.Name FIELD_NAME_INTERVAL = new hydra.core.Name("interval");
  
  private PredefinedType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(String_ instance) ;
    
    R visit(NationalString instance) ;
    
    R visit(Blob instance) ;
    
    R visit(Numeric instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(Datetime instance) ;
    
    R visit(Interval instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PredefinedType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(NationalString instance) {
      return otherwise((instance));
    }
    
    default R visit(Blob instance) {
      return otherwise((instance));
    }
    
    default R visit(Numeric instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Datetime instance) {
      return otherwise((instance));
    }
    
    default R visit(Interval instance) {
      return otherwise((instance));
    }
  }
  
  public static final class String_ extends hydra.ext.org.ansi.sql.syntax.PredefinedType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.PredefinedType_String value;
    
    public String_ (hydra.ext.org.ansi.sql.syntax.PredefinedType_String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
  
  public static final class NationalString extends hydra.ext.org.ansi.sql.syntax.PredefinedType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString value;
    
    public NationalString (hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NationalString)) {
        return false;
      }
      NationalString o = (NationalString) (other);
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
  
  public static final class Blob extends hydra.ext.org.ansi.sql.syntax.PredefinedType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.BinaryLargeObjectStringType value;
    
    public Blob (hydra.ext.org.ansi.sql.syntax.BinaryLargeObjectStringType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Blob)) {
        return false;
      }
      Blob o = (Blob) (other);
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
  
  public static final class Numeric extends hydra.ext.org.ansi.sql.syntax.PredefinedType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.NumericType value;
    
    public Numeric (hydra.ext.org.ansi.sql.syntax.NumericType value) {
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
  
  public static final class Boolean_ extends hydra.ext.org.ansi.sql.syntax.PredefinedType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.BooleanType value;
    
    public Boolean_ (hydra.ext.org.ansi.sql.syntax.BooleanType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Boolean_)) {
        return false;
      }
      Boolean_ o = (Boolean_) (other);
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
  
  public static final class Datetime extends hydra.ext.org.ansi.sql.syntax.PredefinedType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.DatetimeType value;
    
    public Datetime (hydra.ext.org.ansi.sql.syntax.DatetimeType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Datetime)) {
        return false;
      }
      Datetime o = (Datetime) (other);
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
  
  public static final class Interval extends hydra.ext.org.ansi.sql.syntax.PredefinedType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.IntervalType value;
    
    public Interval (hydra.ext.org.ansi.sql.syntax.IntervalType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interval)) {
        return false;
      }
      Interval o = (Interval) (other);
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