// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class DatetimeLiteral implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.DatetimeLiteral");
  
  public static final hydra.core.Name FIELD_NAME_DATE = new hydra.core.Name("date");
  
  public static final hydra.core.Name FIELD_NAME_TIME = new hydra.core.Name("time");
  
  public static final hydra.core.Name FIELD_NAME_TIMESTAMP = new hydra.core.Name("timestamp");
  
  private DatetimeLiteral () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Date instance) ;
    
    R visit(Time instance) ;
    
    R visit(Timestamp instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DatetimeLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Date instance) {
      return otherwise((instance));
    }
    
    default R visit(Time instance) {
      return otherwise((instance));
    }
    
    default R visit(Timestamp instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Date extends hydra.ext.org.ansi.sql.syntax.DatetimeLiteral implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.DateLiteral value;
    
    public Date (hydra.ext.org.ansi.sql.syntax.DateLiteral value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Date)) {
        return false;
      }
      Date o = (Date) (other);
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
  
  public static final class Time extends hydra.ext.org.ansi.sql.syntax.DatetimeLiteral implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.TimeLiteral value;
    
    public Time (hydra.ext.org.ansi.sql.syntax.TimeLiteral value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Time)) {
        return false;
      }
      Time o = (Time) (other);
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
  
  public static final class Timestamp extends hydra.ext.org.ansi.sql.syntax.DatetimeLiteral implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.TimestampLiteral value;
    
    public Timestamp (hydra.ext.org.ansi.sql.syntax.TimestampLiteral value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Timestamp)) {
        return false;
      }
      Timestamp o = (Timestamp) (other);
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