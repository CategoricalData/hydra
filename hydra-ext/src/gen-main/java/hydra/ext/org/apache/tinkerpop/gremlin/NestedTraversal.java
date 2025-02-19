// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class NestedTraversal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal");
  
  public static final hydra.core.Name FIELD_NAME_ROOT = new hydra.core.Name("root");
  
  public static final hydra.core.Name FIELD_NAME_CHAINED = new hydra.core.Name("chained");
  
  public static final hydra.core.Name FIELD_NAME_ANONYMOUS = new hydra.core.Name("anonymous");
  
  private NestedTraversal () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Root instance) ;
    
    R visit(Chained instance) ;
    
    R visit(Anonymous instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NestedTraversal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Root instance) {
      return otherwise((instance));
    }
    
    default R visit(Chained instance) {
      return otherwise((instance));
    }
    
    default R visit(Anonymous instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Root extends hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal value;
    
    public Root (hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Root)) {
        return false;
      }
      Root o = (Root) (other);
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
  
  public static final class Chained extends hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversal value;
    
    public Chained (hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Chained)) {
        return false;
      }
      Chained o = (Chained) (other);
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
  
  public static final class Anonymous extends hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversal value;
    
    public Anonymous (hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Anonymous)) {
        return false;
      }
      Anonymous o = (Anonymous) (other);
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