// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class SingleQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.SingleQuery");
  
  public static final hydra.core.Name FIELD_NAME_SINGLE_PART = new hydra.core.Name("singlePart");
  
  public static final hydra.core.Name FIELD_NAME_MULTI_PART = new hydra.core.Name("multiPart");
  
  private SingleQuery () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(SinglePart instance) ;
    
    R visit(MultiPart instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SingleQuery instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(SinglePart instance) {
      return otherwise((instance));
    }
    
    default R visit(MultiPart instance) {
      return otherwise((instance));
    }
  }
  
  public static final class SinglePart extends hydra.ext.cypher.openCypher.SingleQuery implements Serializable {
    public final hydra.ext.cypher.openCypher.SinglePartQuery value;
    
    public SinglePart (hydra.ext.cypher.openCypher.SinglePartQuery value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SinglePart)) {
        return false;
      }
      SinglePart o = (SinglePart) (other);
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
  
  public static final class MultiPart extends hydra.ext.cypher.openCypher.SingleQuery implements Serializable {
    public final hydra.ext.cypher.openCypher.MultiPartQuery value;
    
    public MultiPart (hydra.ext.cypher.openCypher.MultiPartQuery value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiPart)) {
        return false;
      }
      MultiPart o = (MultiPart) (other);
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