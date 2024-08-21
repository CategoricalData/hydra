// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalSackMethodArgumentOrIntegerArgument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/gremlin.TraversalSackMethodArgumentOrIntegerArgument");
  
  public static final hydra.core.Name FIELD_NAME_CONSUMER = new hydra.core.Name("consumer");
  
  public static final hydra.core.Name FIELD_NAME_INT = new hydra.core.Name("int");
  
  private TraversalSackMethodArgumentOrIntegerArgument () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Consumer instance) ;
    
    R visit(Int instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalSackMethodArgumentOrIntegerArgument instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Consumer instance) {
      return otherwise((instance));
    }
    
    default R visit(Int instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Consumer extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgument value;
    
    public Consumer (hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Consumer)) {
        return false;
      }
      Consumer o = (Consumer) (other);
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
  
  public static final class Int extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument value;
    
    public Int (hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) (other);
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