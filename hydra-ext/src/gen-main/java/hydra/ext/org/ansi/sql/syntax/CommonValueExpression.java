// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class CommonValueExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.CommonValueExpression");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC = new hydra.core.Name("numeric");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_DATETIME = new hydra.core.Name("datetime");
  
  public static final hydra.core.Name FIELD_NAME_INTERVAL = new hydra.core.Name("interval");
  
  public static final hydra.core.Name FIELD_NAME_USER_DEFINED = new hydra.core.Name("userDefined");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCE = new hydra.core.Name("reference");
  
  public static final hydra.core.Name FIELD_NAME_COLLECTION = new hydra.core.Name("collection");
  
  private CommonValueExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Numeric instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Datetime instance) ;
    
    R visit(Interval instance) ;
    
    R visit(UserDefined instance) ;
    
    R visit(Reference instance) ;
    
    R visit(Collection instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CommonValueExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Numeric instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Datetime instance) {
      return otherwise((instance));
    }
    
    default R visit(Interval instance) {
      return otherwise((instance));
    }
    
    default R visit(UserDefined instance) {
      return otherwise((instance));
    }
    
    default R visit(Reference instance) {
      return otherwise((instance));
    }
    
    default R visit(Collection instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Numeric extends hydra.ext.org.ansi.sql.syntax.CommonValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.NumericValueExpression value;
    
    public Numeric (hydra.ext.org.ansi.sql.syntax.NumericValueExpression value) {
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
  
  public static final class String_ extends hydra.ext.org.ansi.sql.syntax.CommonValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.StringValueExpression value;
    
    public String_ (hydra.ext.org.ansi.sql.syntax.StringValueExpression value) {
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
  
  public static final class Datetime extends hydra.ext.org.ansi.sql.syntax.CommonValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.DatetimeValueExpression value;
    
    public Datetime (hydra.ext.org.ansi.sql.syntax.DatetimeValueExpression value) {
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
  
  public static final class Interval extends hydra.ext.org.ansi.sql.syntax.CommonValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.IntervalValueExpression value;
    
    public Interval (hydra.ext.org.ansi.sql.syntax.IntervalValueExpression value) {
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
  
  public static final class UserDefined extends hydra.ext.org.ansi.sql.syntax.CommonValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.UserDefinedTypeValueExpression value;
    
    public UserDefined (hydra.ext.org.ansi.sql.syntax.UserDefinedTypeValueExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UserDefined)) {
        return false;
      }
      UserDefined o = (UserDefined) (other);
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
  
  public static final class Reference extends hydra.ext.org.ansi.sql.syntax.CommonValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.ReferenceValueExpression value;
    
    public Reference (hydra.ext.org.ansi.sql.syntax.ReferenceValueExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reference)) {
        return false;
      }
      Reference o = (Reference) (other);
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
  
  public static final class Collection extends hydra.ext.org.ansi.sql.syntax.CommonValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.CollectionValueExpression value;
    
    public Collection (hydra.ext.org.ansi.sql.syntax.CollectionValueExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Collection)) {
        return false;
      }
      Collection o = (Collection) (other);
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