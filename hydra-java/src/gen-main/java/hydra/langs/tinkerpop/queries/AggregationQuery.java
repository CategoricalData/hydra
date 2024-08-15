// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public abstract class AggregationQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.AggregationQuery");
  
  public static final hydra.core.Name FIELD_NAME_COUNT = new hydra.core.Name("count");
  
  private AggregationQuery () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Count instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AggregationQuery instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Count instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Count extends hydra.langs.tinkerpop.queries.AggregationQuery implements Serializable {
    public Count () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Count)) {
        return false;
      }
      Count o = (Count) (other);
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