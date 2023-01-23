package hydra.ext.sql.ansi;

public abstract class ValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ValueExpression");
  
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
  
  public static final class Common extends hydra.ext.sql.ansi.ValueExpression {
    public final hydra.ext.sql.ansi.CommonValueExpression value;
    
    public Common (hydra.ext.sql.ansi.CommonValueExpression value) {
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
  
  public static final class Boolean_ extends hydra.ext.sql.ansi.ValueExpression {
    public final hydra.ext.sql.ansi.BooleanValueExpression value;
    
    public Boolean_ (hydra.ext.sql.ansi.BooleanValueExpression value) {
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
  
  public static final class Row extends hydra.ext.sql.ansi.ValueExpression {
    public final hydra.ext.sql.ansi.RowValueExpression value;
    
    public Row (hydra.ext.sql.ansi.RowValueExpression value) {
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