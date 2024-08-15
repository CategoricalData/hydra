// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class Query implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.Query");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL_SOURCE = new hydra.core.Name("traversalSource");
  
  public static final hydra.core.Name FIELD_NAME_ROOT_TRAVERSAL = new hydra.core.Name("rootTraversal");
  
  public static final hydra.core.Name FIELD_NAME_TO_STRING = new hydra.core.Name("toString");
  
  public static final hydra.core.Name FIELD_NAME_EMPTY = new hydra.core.Name("empty");
  
  private Query () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(TraversalSource instance) ;
    
    R visit(RootTraversal instance) ;
    
    R visit(ToString instance) ;
    
    R visit(Empty instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Query instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(TraversalSource instance) {
      return otherwise((instance));
    }
    
    default R visit(RootTraversal instance) {
      return otherwise((instance));
    }
    
    default R visit(ToString instance) {
      return otherwise((instance));
    }
    
    default R visit(Empty instance) {
      return otherwise((instance));
    }
  }
  
  public static final class TraversalSource extends hydra.langs.tinkerpop.gremlin.Query implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalSourceQuery value;
    
    public TraversalSource (hydra.langs.tinkerpop.gremlin.TraversalSourceQuery value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalSource)) {
        return false;
      }
      TraversalSource o = (TraversalSource) (other);
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
  
  public static final class RootTraversal extends hydra.langs.tinkerpop.gremlin.Query implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.RootTraversalQuery value;
    
    public RootTraversal (hydra.langs.tinkerpop.gremlin.RootTraversalQuery value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RootTraversal)) {
        return false;
      }
      RootTraversal o = (RootTraversal) (other);
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
  
  public static final class ToString extends hydra.langs.tinkerpop.gremlin.Query implements Serializable {
    public ToString () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToString)) {
        return false;
      }
      ToString o = (ToString) (other);
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
  
  public static final class Empty extends hydra.langs.tinkerpop.gremlin.Query implements Serializable {
    public Empty () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Empty)) {
        return false;
      }
      Empty o = (Empty) (other);
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