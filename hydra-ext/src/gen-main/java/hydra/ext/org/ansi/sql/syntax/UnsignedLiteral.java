// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class UnsignedLiteral implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.UnsignedLiteral");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC = new hydra.core.Name("numeric");
  
  public static final hydra.core.Name FIELD_NAME_GENERAL = new hydra.core.Name("general");
  
  private UnsignedLiteral () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Numeric instance) ;
    
    R visit(General instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnsignedLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Numeric instance) {
      return otherwise((instance));
    }
    
    default R visit(General instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Numeric extends hydra.ext.org.ansi.sql.syntax.UnsignedLiteral implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.UnsignedNumericLiteral value;
    
    public Numeric (hydra.ext.org.ansi.sql.syntax.UnsignedNumericLiteral value) {
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
  
  public static final class General extends hydra.ext.org.ansi.sql.syntax.UnsignedLiteral implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.GeneralLiteral value;
    
    public General (hydra.ext.org.ansi.sql.syntax.GeneralLiteral value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof General)) {
        return false;
      }
      General o = (General) (other);
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