// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class BooleanTerm implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.BooleanTerm");
  
  public static final hydra.core.Name FIELD_NAME_FACTOR = new hydra.core.Name("factor");
  
  public static final hydra.core.Name FIELD_NAME_AND = new hydra.core.Name("and");
  
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
  
  public static final class Factor extends hydra.ext.org.ansi.sql.syntax.BooleanTerm implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.BooleanFactor value;
    
    public Factor (hydra.ext.org.ansi.sql.syntax.BooleanFactor value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class And extends hydra.ext.org.ansi.sql.syntax.BooleanTerm implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.BooleanTerm_And value;
    
    public And (hydra.ext.org.ansi.sql.syntax.BooleanTerm_And value) {
      java.util.Objects.requireNonNull((value));
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