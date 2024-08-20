// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class ValueExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.ValueExpression");
  
  public static final hydra.core.Name FIELD_NAME_COMMON = new hydra.core.Name("common");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_ROW = new hydra.core.Name("row");
  
  private ValueExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Common instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(Row instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Common instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Row instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Common extends hydra.ext.org.ansi.sql.syntax.ValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.CommonValueExpression value;
    
    public Common (hydra.ext.org.ansi.sql.syntax.CommonValueExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Common)) {
        return false;
      }
      Common o = (Common) (other);
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
  
  public static final class Boolean_ extends hydra.ext.org.ansi.sql.syntax.ValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.BooleanValueExpression value;
    
    public Boolean_ (hydra.ext.org.ansi.sql.syntax.BooleanValueExpression value) {
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
  
  public static final class Row extends hydra.ext.org.ansi.sql.syntax.ValueExpression implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.RowValueExpression value;
    
    public Row (hydra.ext.org.ansi.sql.syntax.RowValueExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Row)) {
        return false;
      }
      Row o = (Row) (other);
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