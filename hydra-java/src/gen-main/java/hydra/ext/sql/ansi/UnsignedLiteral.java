package hydra.ext.sql.ansi;

public abstract class UnsignedLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.UnsignedLiteral");
  
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
  
  public static final class Numeric extends hydra.ext.sql.ansi.UnsignedLiteral {
    public final hydra.ext.sql.ansi.UnsignedNumericLiteral value;
    
    public Numeric (hydra.ext.sql.ansi.UnsignedNumericLiteral value) {
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
  
  public static final class General extends hydra.ext.sql.ansi.UnsignedLiteral {
    public final hydra.ext.sql.ansi.GeneralLiteral value;
    
    public General (hydra.ext.sql.ansi.GeneralLiteral value) {
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