// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public abstract class UnaryOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.UnaryOperator");
  
  public static final hydra.core.Name FIELD_NAME_NEGATE = new hydra.core.Name("negate");
  
  private UnaryOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Negate instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Negate instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Negate extends hydra.langs.tinkerpop.queries.UnaryOperator implements Serializable {
    public Negate () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Negate)) {
        return false;
      }
      Negate o = (Negate) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}