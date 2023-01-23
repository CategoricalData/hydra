package hydra.ext.sql.ansi;

public abstract class ValueExpressionPrimary {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ValueExpressionPrimary");
  
  private ValueExpressionPrimary () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Parens instance) ;
    
    R visit(Noparens instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueExpressionPrimary instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Parens instance) {
      return otherwise((instance));
    }
    
    default R visit(Noparens instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Parens extends hydra.ext.sql.ansi.ValueExpressionPrimary {
    public final hydra.ext.sql.ansi.ParenthesizedValueExpression value;
    
    public Parens (hydra.ext.sql.ansi.ParenthesizedValueExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) (other);
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
  
  public static final class Noparens extends hydra.ext.sql.ansi.ValueExpressionPrimary {
    public final hydra.ext.sql.ansi.NonparenthesizedValueExpressionPrimary value;
    
    public Noparens (hydra.ext.sql.ansi.NonparenthesizedValueExpressionPrimary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Noparens)) {
        return false;
      }
      Noparens o = (Noparens) (other);
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