// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class ValueExpressionPrimary implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.ValueExpressionPrimary");
  
  public static final hydra.core.Name FIELD_NAME_PARENS = new hydra.core.Name("parens");
  
  public static final hydra.core.Name FIELD_NAME_NOPARENS = new hydra.core.Name("noparens");
  
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
  
  public static final class Parens extends hydra.ext.org.ansi.sql.syntax.ValueExpressionPrimary implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.ParenthesizedValueExpression value;
    
    public Parens (hydra.ext.org.ansi.sql.syntax.ParenthesizedValueExpression value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Noparens extends hydra.ext.org.ansi.sql.syntax.ValueExpressionPrimary implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary value;
    
    public Noparens (hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary value) {
      java.util.Objects.requireNonNull((value));
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