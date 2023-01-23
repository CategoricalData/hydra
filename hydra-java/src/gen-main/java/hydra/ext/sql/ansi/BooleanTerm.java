package hydra.ext.sql.ansi;

public abstract class BooleanTerm {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.BooleanTerm");
  
  private BooleanTerm () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Factor instance) ;
    
    R visit(And instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BooleanTerm instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Factor instance) {
      return otherwise((instance));
    }
    
    default R visit(And instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Factor extends hydra.ext.sql.ansi.BooleanTerm {
    public final hydra.ext.sql.ansi.BooleanFactor value;
    
    public Factor (hydra.ext.sql.ansi.BooleanFactor value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Factor)) {
        return false;
      }
      Factor o = (Factor) (other);
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
  
  public static final class And extends hydra.ext.sql.ansi.BooleanTerm {
    public final hydra.ext.sql.ansi.BooleanTerm_And value;
    
    public And (hydra.ext.sql.ansi.BooleanTerm_And value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) (other);
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